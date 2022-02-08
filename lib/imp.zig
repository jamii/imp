pub const util = @import("./imp/util.zig");

const std = @import("std");
const u = util;

pub const ExprId = u.Id("e");

const Program = struct {
    rules: []const Rule,

    pub const format = u.formatViaDump;
};

const Rule = struct {
    head: Clause,
    body: []const Clause,

    pub const format = u.formatViaDump;
};

const Clause = struct {
    set_name: Name,
    args: []const Arg,

    pub const format = u.formatViaDump;
};

const Arg = union(enum) {
    Constant: Atom,
    Variable: Name,

    pub const format = u.formatViaDump;
};

const Atom = union(enum) {
    Text: []const u8,
    Number: f64,

    pub const format = u.formatViaDump;
};

// ascii
const Name = []const u8;

// ---

const Token = union(enum) {
    Name: Name,
    OpenParen,
    CloseParen,
    Number: f64,
    Text: []const u8,
    Comma,
    Period,
    Space,
    Def,

    EOF,
};

const TokenTag = @typeInfo(Token).Union.tag_type.?;

const Tokenizer = struct {
    arena: *u.ArenaAllocator,
    source: []const u8,
    position: usize,
    error_info: *?ErrorInfo,

    pub const Error = error{
        // sets error_info
        TokenizerError,

        // does not set error_info
        OutOfMemory,
    };

    pub const ErrorInfo = struct {
        start: usize,
        end: usize,
        message: []const u8,
    };

    fn setError(self: *Tokenizer, start: usize, comptime fmt: []const u8, args: anytype) Error {
        const message = try u.formatToString(self.arena.allocator(), fmt, args);
        self.error_info.* = ErrorInfo{
            .start = start,
            .end = self.position,
            .message = message,
        };
        return error.TokenizerError;
    }

    // using this instead of std.unicode.Utf8Iterator because we want to return the position of any unicode error
    fn nextUtf8Char(self: *Tokenizer) !?u21 {
        const start = self.position;
        if (self.position >= self.source.len) {
            return null;
        }
        const len = std.unicode.utf8ByteSequenceLength(self.source[self.position]) catch |err| {
            self.position += 1;
            return self.setError(start, "{s}", .{err});
        };
        if (self.position + len > self.source.len) {
            return self.setError(start, "{s}", .{error.InvalidUtf8});
        }
        const char = std.unicode.utf8Decode(self.source[self.position .. self.position + len]) catch |err| {
            self.position += len;
            return self.setError(start, "{s}", .{err});
        };
        self.position += len;
        return char;
    }

    fn nextAsciiChar(self: *Tokenizer) !?u7 {
        const start = self.position;
        if (try self.nextUtf8Char()) |char| {
            const ascii_char = @truncate(u7, char);
            if (ascii_char != char) {
                return self.setError(start, "Unicode characters may only appear in texts and comments", .{});
            }
            return ascii_char;
        } else {
            return null;
        }
    }

    // called after seeing '"'
    fn nextText(self: *Tokenizer) ![]const u8 {
        var bytes = u.ArrayList(u8).init(self.arena.allocator());
        const text_start = self.position - 1;
        while (true) {
            const char_start = self.position;
            if (try self.nextUtf8Char()) |char| {
                switch (char) {
                    '\\' => {
                        if (try self.nextUtf8Char()) |escaped_char| {
                            switch (escaped_char) {
                                '"' => try bytes.append('"'),
                                'n' => try bytes.append('\n'),
                                else => return self.setError(char_start, "invalid text escape", .{}),
                            }
                        } else {
                            return self.setError(char_start, "unfinished text escape", .{});
                        }
                    },
                    '"', '\n' => {
                        break;
                    },
                    else => {
                        var char_bytes = [4]u8{ 0, 0, 0, 0 };
                        const len = std.unicode.utf8Encode(char, &char_bytes)
                        // we got this char from utf8Decode so it must be legit
                        catch unreachable;
                        try bytes.appendSlice(char_bytes[0..len]);
                    },
                }
            } else {
                return self.setError(text_start, "unfinished text", .{});
            }
        }
        return bytes.items;
    }

    // called after seeing first char of name
    // name = alpha (alpha | digit | "_")*
    fn nextName(self: *Tokenizer) ![]const u8 {
        const start = self.position - 1;
        while (true) {
            const position = self.position;
            if (try self.nextAsciiChar()) |char| {
                if (!(std.ascii.isAlpha(char) or std.ascii.isDigit(char) or char == '_')) {
                    self.position = position;
                    break;
                }
            } else {
                break;
            }
        }
        return self.source[start..self.position];
    }

    // called after seeing first digit of number
    // number = digit+ ("." digit+)?
    fn nextNumber(self: *Tokenizer) !f64 {
        const start = self.position - 1;
        // first set of digits
        while (true) {
            const position = self.position;
            if (try self.nextAsciiChar()) |char| {
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
            if ((try self.nextAsciiChar()) orelse 0 == '.') {
                break :point true;
            } else {
                self.position = position;
                break :point false;
            }
        };
        const point_end = self.position;
        if (has_decimal_point) {
            // second set of digits
            while (true) {
                const position = self.position;
                if (try self.nextAsciiChar()) |char| {
                    if (!std.ascii.isDigit(char)) {
                        self.position = position;
                        break;
                    }
                } else {
                    break;
                }
            }
        }
        if (has_decimal_point and self.position == point_end) {
            return self.setError(start, "there must be at least one digit after a decimal point", .{});
        } else {
            return std.fmt.parseFloat(f64, self.source[start..self.position]) catch |err|
                return self.setError(start, "{s}", .{err});
        }
    }

    pub fn nextToken(self: *Tokenizer) !Token {
        const start = self.position;
        if (try self.nextUtf8Char()) |utf8_char1| {
            switch (utf8_char1) {
                '(' => return Token{ .OpenParen = {} },
                ')' => return Token{ .CloseParen = {} },
                '"' => return Token{ .Text = try self.nextText() },
                ',' => return Token{ .Comma = {} },
                '.' => return Token{ .Period = {} },
                '<' => {
                    if ((try self.nextAsciiChar()) orelse 0 == '-') {
                        return Token{ .Def = {} };
                    } else {
                        return self.setError(start, "invalid token", .{});
                    }
                },
                else => {
                    const ascii_char1 = @intCast(u8, utf8_char1 & 0b0111_1111);
                    if (utf8_char1 != @as(u21, ascii_char1)) {
                        return self.setError(start, "unicode characters may only appear in texts and comments", .{});
                    } else if (std.ascii.isSpace(ascii_char1)) {
                        return Token{ .Space = {} };
                    } else if (std.ascii.isDigit(ascii_char1)) {
                        return Token{ .Number = try self.nextNumber() };
                    } else if (std.ascii.isAlpha(ascii_char1)) {
                        return Token{ .Name = try self.nextName() };
                    } else {
                        return self.setError(start, "invalid token", .{});
                    }
                },
            }
        } else {
            return Token{ .EOF = {} };
        }
    }
};

fn tokenize(arena: *u.ArenaAllocator, source: []const u8, error_info: *?Tokenizer.ErrorInfo) Tokenizer.Error![]const Token {
    var tokenizer = Tokenizer{
        .arena = arena,
        .source = source,
        .position = 0,
        .error_info = error_info,
    };
    var tokens = u.ArrayList(Token).init(arena.allocator());
    while (true) {
        const token = try tokenizer.nextToken();
        try tokens.append(token);
        if (token == .EOF) break;
    }
    return tokens.toOwnedSlice();
}

const Parser = struct {
    arena: *u.ArenaAllocator,
    tokens: []const Token,
    position: usize,
    error_info: *?ErrorInfo,

    pub const Error = error{
        // sets error_info
        ParserError,

        // does not set error_info
        OutOfMemory,
    };

    pub const ErrorInfo = struct {
        start: usize,
        end: usize,
        message: []const u8,
    };

    fn setError(self: *Parser, start: usize, comptime fmt: []const u8, args: anytype) Error {
        const message = try u.formatToString(self.arena.allocator(), fmt, args);
        self.error_info.* = ErrorInfo{
            .start = start,
            .end = self.position,
            .message = message,
        };
        return error.ParserError;
    }

    fn peekToken(self: *Parser) Token {
        return self.tokens[self.position];
    }

    fn nextToken(self: *Parser) Token {
        const token = self.peekToken();
        self.position += 1;
        return token;
    }

    fn expectToken(self: *Parser, expected: TokenTag) !Token {
        const start = self.position;
        const found = self.nextToken();
        if (found != expected)
            return self.setError(start, "Expected {}, found {}", .{ expected, found })
        else
            return found;
    }

    fn allowSpace(self: *Parser) void {
        while (self.peekToken() == .Space)
            _ = self.nextToken();
    }

    pub fn parseProgram(self: *Parser) !Program {
        var rules = u.ArrayList(Rule).init(self.arena.allocator());
        while (true) {
            self.allowSpace();
            if (self.peekToken() == .EOF) break;
            try rules.append(try self.parseRule());
            self.allowSpace();
        }
        return Program{ .rules = rules.toOwnedSlice() };
    }

    pub fn parseRule(self: *Parser) !Rule {
        const head = try self.parseClause();
        self.allowSpace();
        if (self.peekToken() == .Period) {
            _ = try self.expectToken(.Period);
            return Rule{ .head = head, .body = &.{} };
        }
        _ = try self.expectToken(.Def);
        self.allowSpace();
        var body = u.ArrayList(Clause).init(self.arena.allocator());
        while (true) {
            self.allowSpace();
            try body.append(try self.parseClause());
            self.allowSpace();
            if (self.peekToken() == .Period) break;
            _ = try self.expectToken(.Comma);
        }
        _ = try self.expectToken(.Period);
        return Rule{ .head = head, .body = body.toOwnedSlice() };
    }

    pub fn parseClause(self: *Parser) !Clause {
        const set_name = try self.expectToken(.Name);
        _ = try self.expectToken(.OpenParen);
        var args = u.ArrayList(Arg).init(self.arena.allocator());
        while (true) {
            self.allowSpace();
            if (self.peekToken() == .CloseParen) break;
            try args.append(try self.parseArg());
            self.allowSpace();
            if (self.peekToken() == .CloseParen) break;
            _ = try self.expectToken(.Comma);
        }
        _ = try self.expectToken(.CloseParen);
        return Clause{ .set_name = set_name.Name, .args = args.toOwnedSlice() };
    }

    pub fn parseArg(self: *Parser) !Arg {
        const start = self.position;
        const token = self.nextToken();
        switch (token) {
            .Name => |name| return Arg{ .Variable = name },
            .Number => |number| return Arg{ .Constant = .{ .Number = number } },
            .Text => |text| return Arg{ .Constant = .{ .Text = text } },
            else => return self.setError(start, "Expected variable or atom, found {}", .{token}),
        }
    }
};

fn parse(arena: *u.ArenaAllocator, tokens: []const Token, error_info: *?Parser.ErrorInfo) Parser.Error!Program {
    var parser = Parser{
        .arena = arena,
        .tokens = tokens,
        .position = 0,
        .error_info = error_info,
    };
    return parser.parseProgram();
}

comptime {
    std.testing.refAllDecls(@This());
}

fn testTokenizeAndParse(source: []const u8, expected: []const u8) !void {
    var arena = u.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var tokenizer_error_info: ?Tokenizer.ErrorInfo = null;
    var parser_error_info: ?Parser.ErrorInfo = null;
    const found = found: {
        const tokens = tokenize(&arena, source, &tokenizer_error_info) catch |err|
            break :found try u.formatToString(
            arena.allocator(),
            "TokenizerError:\n{}\n{s}\nAt {}:{}",
            .{ err, tokenizer_error_info.?.message, tokenizer_error_info.?.start, tokenizer_error_info.?.end },
        );
        const program = parse(&arena, tokens, &parser_error_info) catch |err|
            break :found try u.formatToString(
            arena.allocator(),
            "ParserError:\n{}\n{s}\nAt {}:{}",
            .{ err, parser_error_info.?.message, parser_error_info.?.start, parser_error_info.?.end },
        );
        break :found try u.formatToString(arena.allocator(), "{}", .{program});
    };
    try std.testing.expectEqualStrings(expected, found);
}

test {
    try testTokenizeAndParse(
        \\
    ,
        \\Program{
        \\    .rules = []Rule[
        \\    ],
        \\}
    );

    try testTokenizeAndParse(
        \\parent("Alice", "Bob").
        \\parent("Bob", "Eve").
        \\age("Eve", 101).
        \\weight("Eve", 42.7).
        \\ancestor(x,z) <- parent(x,y), ancestor(y,z).
    ,
        \\Program{
        \\    .rules = []Rule[
        \\        Rule{
        \\            .head = Clause{
        \\                .set_name = "parent",
        \\                .args = []Arg[
        \\                    Arg{
        \\                        .Constant = Atom{
        \\                            .Text = "Alice"
        \\                        }
        \\                    },
        \\                    Arg{
        \\                        .Constant = Atom{
        \\                            .Text = "Bob"
        \\                        }
        \\                    },
        \\                ],
        \\            },
        \\            .body = []Clause[
        \\            ],
        \\        },
        \\        Rule{
        \\            .head = Clause{
        \\                .set_name = "parent",
        \\                .args = []Arg[
        \\                    Arg{
        \\                        .Constant = Atom{
        \\                            .Text = "Bob"
        \\                        }
        \\                    },
        \\                    Arg{
        \\                        .Constant = Atom{
        \\                            .Text = "Eve"
        \\                        }
        \\                    },
        \\                ],
        \\            },
        \\            .body = []Clause[
        \\            ],
        \\        },
        \\        Rule{
        \\            .head = Clause{
        \\                .set_name = "age",
        \\                .args = []Arg[
        \\                    Arg{
        \\                        .Constant = Atom{
        \\                            .Text = "Eve"
        \\                        }
        \\                    },
        \\                    Arg{
        \\                        .Constant = Atom{
        \\                            .Number = 1.01e+02
        \\                        }
        \\                    },
        \\                ],
        \\            },
        \\            .body = []Clause[
        \\            ],
        \\        },
        \\        Rule{
        \\            .head = Clause{
        \\                .set_name = "weight",
        \\                .args = []Arg[
        \\                    Arg{
        \\                        .Constant = Atom{
        \\                            .Text = "Eve"
        \\                        }
        \\                    },
        \\                    Arg{
        \\                        .Constant = Atom{
        \\                            .Number = 4.27e+01
        \\                        }
        \\                    },
        \\                ],
        \\            },
        \\            .body = []Clause[
        \\            ],
        \\        },
        \\        Rule{
        \\            .head = Clause{
        \\                .set_name = "ancestor",
        \\                .args = []Arg[
        \\                    Arg{
        \\                        .Variable = "x"
        \\                    },
        \\                    Arg{
        \\                        .Variable = "z"
        \\                    },
        \\                ],
        \\            },
        \\            .body = []Clause[
        \\                Clause{
        \\                    .set_name = "parent",
        \\                    .args = []Arg[
        \\                        Arg{
        \\                            .Variable = "x"
        \\                        },
        \\                        Arg{
        \\                            .Variable = "y"
        \\                        },
        \\                    ],
        \\                },
        \\                Clause{
        \\                    .set_name = "ancestor",
        \\                    .args = []Arg[
        \\                        Arg{
        \\                            .Variable = "y"
        \\                        },
        \\                        Arg{
        \\                            .Variable = "z"
        \\                        },
        \\                    ],
        \\                },
        \\            ],
        \\        },
        \\    ],
        \\}
    );

    try testTokenizeAndParse(
        \\parent("Alice", "Bob").
        \\parent("Bob", "Eve").
        \\age("Eve", 101).
        \\weight("Eve", 42.7).
        \\ancestor(x,z) <- .
    ,
        \\ParserError:
        \\error.ParserError
        \\Expected @typeInfo(Token).Union.tag_type.?.Name, found Token{ .Period = void }
        \\At 45:46
    );

    try testTokenizeAndParse(
        \\parent("Alice", "Bob").
        \\parent("Bob", "Eve").
        \\age("Eve", 101).
        \\weight("Eve", 42.7).
        \\ancestor(x,z) < parent(x,y), ancestor(y,z).
    ,
        \\TokenizerError:
        \\error.TokenizerError
        \\invalid token
        \\At 98:100
    );
}
