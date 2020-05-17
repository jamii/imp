usingnamespace @import("common.zig");

const expr = @import("./expr.zig");

// TODO https://github.com/ziglang/zig/issues/2647
pub fn parse(arena: *ArenaAllocator, source: []const u8) !Result(expr.Surface, ParseError) {
    var parser = Parser{
        .arena = arena,
        .source = source,
        .position = 0,
        .parse_error = ParseError{
            .start = 0,
            .end = 0,
            .message = "not an error",
        },
    };
    if (parser.expr) |expr| {
        return Result{.Ok = expr};
    } else |err| {
        switch (err) {
            .ParseError => return Result{.Err = ParseError{
                .position = parser.chars.i,
                .message = parser.error_message,
            }},
            .OutOfMemory => return error.OutOfMemory,
        }
    }
}

pub const ParseError = struct {
    start: usize,
    end: usize,
    message: []const u8,
};

// --------------------------------------------------------------------------------

const Token = union(enum) {
    // core
    None,
    Some,
    Number: f64,
    String: []const u8, // valid utf8
    Union,
    Intersect,
    Product,
    Equal,
    Name: []const u8, // ascii, non-empty
    When,
    Abstract,
    Seal,
    Unseal,
    Annotate,

    // sugar
    Negate,
    If,
    OpenBlock,
    CloseBlock,
    Let,
    EndLet,
    Lookup,

    // natives
    Plus,
    Minus,
    Times,
    Divide,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,

    // other
    Comment,
    Whitespace,
    EOF,
};

fn append_char(bytes: *ArrayList(u8), char: u21) !void {
    var char_bytes = [4]u8{0,0,0,0};
    const len = try std.unicode.utf8Encode(char, &char_bytes);
    try bytes.appendSlice(char_bytes[0..len]);
}

const Parser = struct {
    arena: *ArenaAllocator,
    source: []const u8,
    position: usize,
    parse_error: ParseError,

    fn err(self: *Parser, start: usize, end: usize, comptime message: []const u8, args: var) !noreturn {
        self.parse_error = ParseError{
            .start = start,
            .end = end,
            .message = try format(&self.arena.allocator, message, args),
        };
        return error.ParseError;
    }

    fn expect(self: *Parser, expected: Token) !void {
        const start = self.position;
        const found = self.token().token;
        if (found != expected) {
            try self.err(start, self.position, "Expected {}, found {}", .{expected, found});
        }
    }

    // use this instead of std.unicode.Utf8Iterator because we want to return the position of any unicode error
    fn next_utf8_char(self: *Parser) !?u21 {
        if (self.position >= self.source.len) {
            return null;
        }
        const len = try std.unicode.utf8ByteSequenceLength(self.source[self.position]);
        if (self.position + len > self.source.len) {
            return error.InvalidUtf8;
        }
        const char = try std.unicode.utf8Decode(self.source[self.position .. self.position + len]);
        self.position += len;
        dump(char);
        return char;
    }

    fn next_ascii_char(self: *Parser) !?u8 {
        const start = self.position;
        if (try self.next_utf8_char()) |char| {
            const ascii_char = @intCast(u8, char & 0b0111_1111);
            if (@as(u21, char) != ascii_char) {
                try self.err(start, self.position, "unicode characters may only appear in strings and comments", .{});
            }
            return char;
        } else {
            return null;
        }
    }

    // called after seeing '"'
    fn tokenize_string(self: *Parser) ![]const u8 {
        var bytes = ArrayList(u8).init(&self.arena.allocator);
        const string_start = self.position - 1;
        while (true) {
            const char_start = self.position;
            if (self.next_utf8_char()) |char| {
                switch (char) {
                    '\\' => {
                        if (self.next_utf8_char()) |escaped_char| {
                            switch (escaped_char) {
                                '"' => try append_char(bytes, escaped_char),
                                _ => try self.err(char_start, self.position, "invalid string escape", .{}),
                            }
                        } else {
                            try self.err(char_start, self.position, "unfinished string escape", .{});
                        }

                    },
                    '"' => {
                        break;
                    },
                    _ => {
                        try append_char(bytes, char);
                    },
                }
            } else {
                try self.err(string_start, self.position, "unfinished string", .{});
            }
        }
        return bytes.items;
    }

    // called after seeing first char of name
    fn tokenize_name(self: *Parser) ![]const u8 {
        const start = self.position - 1;
        while (true) {
            const position = self.position;
            const char = try self.next_ascii_char();
            if (!std.ascii.isAlpha(char) and !std.ascii.isDigit(char)) {
                self.position = position;
                break;
            }
        }
        return self.source[start..self.position];
    }

    // called after seeing first char of number
    fn tokenize_number(self: *Parser) !f64 {
        const start = self.position - 1;
        // first set of digits
        while (true) {
            const position = self.position;
            const char = try self.next_ascii_char();
            if (!std.ascii.isDigit(char)) {
                self.position = position;
                break;
            }
        }
        // maybe '.'
        const has_decimal_point = point: {
            const position = self.position;
            if (try self.next_ascii_char() == '.') {
                break :point true;
            } else {
                self.position = position;
                break :point false;
            }
        };
        if (has_decimal_point) {
            // second set of digits
            while (true) {
                const position = self.position;
                const char = try self.next_ascii_char();
                if (!std.ascii.isDigit(char)) {
                    self.position = position;
                    break;
                }
            }
        }
        return std.fmt.parseFloat(self.source[start..self.position]);
    }

    fn next_token(self: *Parser) !Token {
        const start = self.position;
        if (try self.next_ascii_char()) |char1| {
            switch (char1) {
                '"' => return .{.String = try self.string()},
                '|' => return .Union,
                '&' => return .Intersect,
                'x' => return .Product,
                '=' => return .Equal,
                '-' => {
                    const position = self.position;
                    if (try self.next_ascii_char() == '>') {
                        return .Abstract;
                    } else {
                        self.position = position;
                        return .Minus;
                    }
                },
                '#' => return .Annotate,
                '!' => return .Negate,
                '{' => return .OpenBlock,
                '}' => return .CloseBlock,
                ';' => return .EndLet,
                ':' => return .Lookup,
                '+' => return .Plus,
                '-' => return .Minus,
                '*' => return .Times,
                '/' => {
                    const position = self.position;
                    if (try self.next_ascii_char() == '/') {
                        while (true) {
                            // utf8 chars are allowed in comments
                            const char2 = self.next_utf8_char();
                            if (char2 == null or char2 == '\n') {
                                break;
                            }
                        }
                        return .Comment;
                    } else {
                        self.position = position;
                        return .Divide;
                    }
                },
                '<' => {
                    const position = self.position;
                    if (try self.next_ascii_char() == '=') {
                        return .LessThanOrEqual;
                    } else {
                        self.position = position;
                        return .LessThan;
                    }
                },
                '>' => {
                    const position = self.position;
                    if (try self.next_ascii_char() == '=') {
                        return .GreaterThanOrEqual;
                    } else {
                        self.position = position;
                        return .GreaterThan;
                    }
                },
                else => {
                    const ascii_char1 = @intCast(u8, char1 & 0b0111_1111);
                    if (char1 != @as(u21, ascii_char1)) {
                        try self.err(start, self.position, "unicode characters may only appear in strings and comments", .{});
                    } else if (std.ascii.isWhiteSpace(ascii_char1)) {
                        return self.next_token();
                    } else if (std.ascii.isAlpha(ascii_char1)) {
                        const name = try self.tokenize_name();
                        switch (name) {
                            "none" => return .None,
                            "some" => return .Some,
                            "when" => return .When,
                            "seal" => return .Seal,
                            "unseal" => return .Unseal,
                            "if" => return .If,
                            "let" => return .Let,
                            else => return .{.Name = name},
                        }
                    } else if (std.ascii.isDigit(ascii_char1)) {
                        return .{.Number = try self.tokenize_number()};
                    } else {
                        try self.err(start, self.position, "invalid token", .{});
                    }
                }
            }
        } else {
            return .EOF;
        }
    }
};

test "compiles" {
    test_compiles();
}

pub fn test_compiles() !void {
    var arena = ArenaAllocator.init(std.testing.allocator);
    const source = "1.3 + 1";
    var parser = Parser{
        .arena = &arena,
        .source = source,
        .position = 0,
        .parse_error = ParseError{
            .start = 0,
            .end = 0,
            .message = "not an error",
        },
    };
    expect(meta.deepEqual(try parser.next_token(), Token{.Number = 1.3}));
}
