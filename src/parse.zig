usingnamespace @import("common.zig");

const expr = @import("./expr.zig");

// TODO https://github.com/ziglang/zig/issues/2647
pub fn parse(arena: *ArenaAllocator, source: []const u8, parse_error_info: ParseErrorInfo) ParseError!expr.Surface {
    var parser = Parser{
        .arena = arena,
        .source = source,
        .position = 0,
        .parse_error_info = parse_error_info,
    };
    try parser.expr();
}

pub const ParseError = error {
    ParseError,
    InvalidUtf8,
    OutOfMemory,
};

pub const ParseErrorInfo = struct {
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
    parse_error_info: ParseErrorInfo,

    fn parse_error(self: *Parser, start: usize, end: usize, comptime fmt: []const u8, args: var) ParseError {
        self.parse_error_info.start = start;
        self.parse_error_info.end = end;
        if (format(&self.arena.allocator, fmt, args)) |message| {
            self.parse_error_info.message = message;
            return error.ParseError;
        } else |err| {
            switch (err) {
                error.OutOfMemory => self.parse_error_info.message = "out of memory",
            }
            return err;
        }
    }

    fn expect(self: *Parser, expected: Token) !void {
        const start = self.position;
        const found = self.token().token;
        if (found != expected) {
            return self.parse_error(start, self.position, "Expected {}, found {}", .{expected, found});
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
        return char;
    }

    fn next_ascii_char(self: *Parser) !?u8 {
        const start = self.position;
        if (try self.next_utf8_char()) |char| {
            const ascii_char = @intCast(u8, char & 0b0111_1111);
            if (@as(u21, char) != ascii_char) {
                return self.parse_error(start, self.position, "unicode characters may only appear in strings and comments", .{});
            }
            return ascii_char;
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
            if (try self.next_utf8_char()) |char| {
                switch (char) {
                    '\\' => {
                        if (try self.next_utf8_char()) |escaped_char| {
                            switch (escaped_char) {
                                '"' => try append_char(&bytes, escaped_char),
                                else => return self.parse_error(char_start, self.position, "invalid string escape", .{}),
                            }
                        } else {
                            return self.parse_error(char_start, self.position, "unfinished string escape", .{});
                        }

                    },
                    '"' => {
                        break;
                    },
                    else => {
                        try append_char(&bytes, char);
                    },
                }
            } else {
                return self.parse_error(string_start, self.position, "unfinished string", .{});
            }
        }
        return bytes.items;
    }

    // called after seeing first char of name
    fn tokenize_name(self: *Parser) ![]const u8 {
        const start = self.position - 1;
        while (true) {
            const position = self.position;
            if (try self.next_ascii_char()) |char| {
                if (!std.ascii.isAlpha(char) and !std.ascii.isDigit(char)) {
                    self.position = position;
                    break;
                }
            } else {
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
            if (try self.next_ascii_char()) |char| {
                if (!std.ascii.isDigit(char)) {
                    self.position = position;
                    break;
                }
            } else {
                break;
            }
        }
        // maybe '.'
        const has_decimal_point = point: {
            const position = self.position;
            if ((try self.next_ascii_char()) orelse 0 == '.') {
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
                if (try self.next_ascii_char()) |char| {
                    if (!std.ascii.isDigit(char)) {
                        self.position = position;
                        break;
                    }
                } else {
                    break;
                }
            }
        }
        return std.fmt.parseFloat(f64, self.source[start..self.position]);
    }

    fn next_token(self: *Parser) !Token {
        const start = self.position;
        if (try self.next_ascii_char()) |char1| {
            switch (char1) {
                '"' => return Token{.String = try self.tokenize_string()},
                '|' => return Token{.Union={}},
                '&' => return Token{.Intersect={}},
                'x' => return Token{.Product={}},
                '=' => return Token{.Equal={}},
                '-' => {
                    const position = self.position;
                    if ((try self.next_ascii_char()) orelse 0 == '>') {
                        return Token{.Abstract={}};
                    } else {
                        self.position = position;
                        return Token{.Minus={}};
                    }
                },
                '#' => return Token{.Annotate={}},
                '!' => return Token{.Negate={}},
                '{' => return Token{.OpenBlock={}},
                '}' => return Token{.CloseBlock={}},
                ';' => return Token{.EndLet={}},
                ':' => return Token{.Lookup={}},
                '+' => return Token{.Plus={}},
                '*' => return Token{.Times={}},
                '/' => {
                    const position = self.position;
                    if ((try self.next_ascii_char()) orelse 0 == '/') {
                        while (true) {
                            // utf8 chars are allowed in comments
                            if (try self.next_utf8_char()) |char2| {
                                if (char2 == '\n') {
                                    break;
                                }
                            } else {
                                break;
                            }
                        }
                        return Token{.Comment={}};
                    } else {
                        self.position = position;
                        return Token{.Divide={}};
                    }
                },
                '<' => {
                    const position = self.position;
                    if ((try self.next_ascii_char()) orelse 0 == '=') {
                        return Token{.LessThanOrEqual={}};
                    } else {
                        self.position = position;
                        return Token{.LessThan={}};
                    }
                },
                '>' => {
                    const position = self.position;
                    if ((try self.next_ascii_char()) orelse 0 == '=') {
                        return Token{.GreaterThanOrEqual={}};
                    } else {
                        self.position = position;
                        return Token{.GreaterThan={}};
                    }
                },
                else => {
                    const ascii_char1 = @intCast(u8, char1 & 0b0111_1111);
                    if (char1 != @as(u21, ascii_char1)) {
                        return self.parse_error(start, self.position, "unicode characters may only appear in strings and comments", .{});
                    } else if (std.ascii.isSpace(ascii_char1)) {
                        return self.next_token();
                    } else if (std.ascii.isAlpha(ascii_char1)) {
                        const name = try self.tokenize_name();
                        if (meta.deepEqual(name, "none")) return Token{.None={}};
                        if (meta.deepEqual(name, "some")) return Token{.Some={}};
                        if (meta.deepEqual(name, "when")) return Token{.When={}};
                        if (meta.deepEqual(name, "seal")) return Token{.Seal={}};
                        if (meta.deepEqual(name, "unseal")) return Token{.Unseal={}};
                        if (meta.deepEqual(name, "if")) return Token{.If={}};
                        if (meta.deepEqual(name, "let")) return Token{.Let={}};
                        return Token{.Name = name};
                    } else if (std.ascii.isDigit(ascii_char1)) {
                        return Token{.Number = try self.tokenize_number()};
                    } else {
                        return self.parse_error(start, self.position, "invalid token", .{});
                    }
                }
            }
        } else {
            return Token{.EOF={}};
        }
    }
};

test "compiles" {
    var arena = ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const source =
        \\ 1.3 + "foo\"bar"
    ;
    var parser = Parser{
        .arena = &arena,
        .source = source,
        .position = 0,
        .parse_error_info = ParseErrorInfo{
            .start = 0,
            .end = 0,
            .message = "not an error",
        },
    };
    expect(meta.deepEqual(try parser.next_token(), Token{.Number = 1.3}));
    expect(meta.deepEqual(try parser.next_token(), Token.Plus));

    expect(meta.deepEqual(try parser.next_token(), Token{.String = "foo\"bar"}));
}
