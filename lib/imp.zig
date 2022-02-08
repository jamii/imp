pub const util = @import("./imp/util.zig");

const std = @import("std");
const u = util;

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

// ---

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

// ---

const ProgramPlan = struct {
    rules: []const RulePlan,

    pub const format = u.formatViaDump;
};

const RulePlan = struct {
    set_name: Name,
    body: PlanExpr,

    pub const format = u.formatViaDump;
};

const PlanExpr = union(enum) {
    Row: struct {
        atoms: []const Atom,
    },
    Scan: struct {
        set_name: Name,
        num_columns: usize,
    },
    FilterConstant: struct {
        in: *const PlanExpr,
        column: usize,
        constant: Atom,
    },
    FilterEqual: struct {
        in: *const PlanExpr,
        columns: [2]usize,
    },
    Product: struct {
        in: [2]*const PlanExpr,
    },
    Project: struct {
        in: *const PlanExpr,
        columns: []const usize,
    },

    pub const format = u.formatViaDump;
};

const Planner = struct {
    arena: *u.ArenaAllocator,
    error_info: *?ErrorInfo,

    pub const Error = error{
        // sets error_info
        PlannerError,

        // does not set error_info
        OutOfMemory,
    };

    pub const ErrorInfo = struct {
        message: []const u8,
    };

    fn setError(self: *Planner, comptime fmt: []const u8, args: anytype) Error {
        const message = try u.formatToString(self.arena.allocator(), fmt, args);
        self.error_info.* = ErrorInfo{
            .message = message,
        };
        return error.PlannerError;
    }

    fn planExpr(self: *Planner, plan_expr: PlanExpr) !*const PlanExpr {
        const ptr = try self.arena.allocator().create(PlanExpr);
        ptr.* = plan_expr;
        return ptr;
    }

    fn planProgram(self: *Planner, program: Program) !ProgramPlan {
        var rules = u.ArrayList(RulePlan).init(self.arena.allocator());
        for (program.rules) |rule|
            try rules.append(try self.planRule(rule));
        return ProgramPlan{ .rules = rules.toOwnedSlice() };
    }

    fn planRule(self: *Planner, rule: Rule) !RulePlan {
        var body = PlanExpr{ .Row = .{ .atoms = &.{} } };
        var body_num_columns: usize = 0;
        // maps each bound variable to a column
        var body_variables = u.DeepHashMap(Name, usize).init(self.arena.allocator());
        for (rule.body) |clause| {

            // handle this clause with scan and filter
            var clause_expr = PlanExpr{ .Scan = .{
                .set_name = clause.set_name,
                .num_columns = clause.args.len,
            } };
            for (clause.args) |arg, column| {
                if (arg == .Constant) {
                    const old_clause_expr = clause_expr;
                    clause_expr = PlanExpr{ .FilterConstant = .{
                        .in = try self.planExpr(old_clause_expr),
                        .column = column,
                        .constant = arg.Constant,
                    } };
                }
            }

            // join against body so far
            {
                const old_body = body;
                body = PlanExpr{ .Product = .{
                    .in = .{
                        try self.planExpr(old_body),
                        try self.planExpr(clause_expr),
                    },
                } };
            }
            for (clause.args) |arg, column| {
                if (arg == .Variable) {
                    if (body_variables.get(arg.Variable)) |join_column| {
                        const old_body = body;
                        body = PlanExpr{ .FilterEqual = .{
                            .in = try self.planExpr(old_body),
                            .columns = .{ join_column, body_num_columns + column },
                        } };
                    } else {
                        try body_variables.put(arg.Variable, body_num_columns + column);
                    }
                }
            }
            body_num_columns += clause.args.len;
        }

        // project variables and constants used in head
        var constants = u.ArrayList(Atom).init(self.arena.allocator());
        var project_columns = u.ArrayList(usize).init(self.arena.allocator());
        for (rule.head.args) |arg| {
            switch (arg) {
                .Variable => |variable| {
                    if (body_variables.get(variable)) |project_column| {
                        try project_columns.append(project_column);
                    } else {
                        return self.setError("Variable {s} is used in head but not bound in body", .{variable});
                    }
                },
                .Constant => |atom| {
                    try project_columns.append(body_num_columns + constants.items.len);
                    try constants.append(atom);
                },
            }
        }
        const old_body = body;
        return RulePlan{
            .set_name = rule.head.set_name,
            .body = PlanExpr{ .Project = .{
                .in = try self.planExpr(.{ .Product = .{
                    .in = .{
                        try self.planExpr(old_body),
                        try self.planExpr(.{ .Row = .{
                            .atoms = constants.toOwnedSlice(),
                        } }),
                    },
                } }),
                .columns = project_columns.toOwnedSlice(),
            } },
        };
    }
};

fn plan(arena: *u.ArenaAllocator, program: Program, error_info: *?Planner.ErrorInfo) Planner.Error!ProgramPlan {
    var planner = Planner{
        .arena = arena,
        .error_info = error_info,
    };
    return planner.planProgram(program);
}

// ---

comptime {
    std.testing.refAllDecls(@This());
}

fn testEndToEnd(source: []const u8, expected: []const u8) !void {
    var arena = u.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var tokenizer_error_info: ?Tokenizer.ErrorInfo = null;
    var parser_error_info: ?Parser.ErrorInfo = null;
    var planner_error_info: ?Planner.ErrorInfo = null;
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
        const program_plan = plan(&arena, program, &planner_error_info) catch |err| break :found try u.formatToString(
            arena.allocator(),
            "PlannerError:\n{}\n{s}",
            .{ err, planner_error_info.?.message },
        );
        break :found try u.formatToString(arena.allocator(), "{}\n---\n{}", .{ program, program_plan });
    };
    try std.testing.expectEqualStrings(expected, found);
}

test {
    const cases = @embedFile("../test/end_to_end.test");
    var case_iter = std.mem.split(u8, cases, "\n\n===\n\n");
    while (case_iter.next()) |case| {
        const sep = "\n---\n";
        const source_end = std.mem.indexOf(u8, case, sep).?;
        const source = case[0..source_end];
        const expected = case[source_end + sep.len ..];
        try testEndToEnd(source, expected);
    }
}
