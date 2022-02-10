pub const util = @import("./imp/util.zig");
pub const Storage = @import("./imp/Storage.zig");

const std = @import("std");
const u = util;

comptime {
    std.testing.refAllDecls(@This());
}

// ascii
const Name = []const u8;

// ---

pub const Token = union(enum) {
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

pub const TokenTag = @typeInfo(Token).Union.tag_type.?;

pub const Tokenizer = struct {
    arena: *u.ArenaAllocator,
    source: []const u8,
    position: usize,
    tokens: u.ArrayList(Token),
    error_info: ?ErrorInfo,

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
        pub const format = u.formatViaDump;
    };

    fn setError(self: *Tokenizer, start: usize, comptime fmt: []const u8, args: anytype) Error {
        const message = try u.formatToString(self.arena.allocator(), fmt, args);
        self.error_info = ErrorInfo{
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

    fn nextToken(self: *Tokenizer) !Token {
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

    pub fn tokenize(self: *Tokenizer) ![]const Token {
        while (true) {
            const token = try self.nextToken();
            try self.tokens.append(token);
            if (token == .EOF) break;
        }
        return self.tokens.items;
    }
};

// ---

pub const Program = struct {
    rules: []const Rule,

    pub const format = u.formatViaDump;
};

pub const Rule = struct {
    head: Clause,
    body: []const Clause,

    pub const format = u.formatViaDump;

    pub fn printInto(self: Rule, writer: anytype) !void {
        try self.head.printInto(writer);
        if (self.body.len > 0) {
            try writer.writeAll(" <-\n");
            for (self.body) |clause, i| {
                if (i != 0) try writer.writeAll(",\n");
                try writer.writeAll("  ");
                try clause.printInto(writer);
            }
        }
        try writer.writeAll(".");
    }
};

pub const Clause = struct {
    set_name: Name,
    args: []const Arg,

    pub const format = u.formatViaDump;

    pub fn printInto(self: Clause, writer: anytype) !void {
        try writer.writeAll(self.set_name);
        try writer.writeAll("(");
        for (self.args) |arg, i| {
            if (i != 0) try writer.writeAll(", ");
            try arg.printInto(writer);
        }
        try writer.writeAll(")");
    }
};

pub const Arg = union(enum) {
    Constant: Atom,
    Variable: Name,

    pub const format = u.formatViaDump;

    pub fn printInto(self: Arg, writer: anytype) !void {
        switch (self) {
            .Constant => |atom| try atom.printInto(writer),
            .Variable => |name| try writer.writeAll(name),
        }
    }
};

pub const Atom = union(enum) {
    Text: []const u8,
    Number: f64,

    pub const format = u.formatViaDump;

    pub fn dumpInto(self: Atom, writer: anytype, indent: u32) u.WriterError(@TypeOf(writer))!void {
        _ = indent;
        switch (self) {
            .Text => |text| try std.fmt.format(writer, "\"{}\"", .{std.zig.fmtEscapes(text)}),
            .Number => |number| try std.fmt.format(writer, "{d}", .{number}),
        }
    }

    pub fn printInto(self: Atom, writer: anytype) !void {
        try self.dumpInto(writer, 0);
    }
};

pub const Parser = struct {
    arena: *u.ArenaAllocator,
    tokens: []const Token,
    position: usize,
    rules: u.ArrayList(Rule),
    error_info: ?ErrorInfo,

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
        pub const format = u.formatViaDump;
    };

    fn setError(self: *Parser, start: usize, comptime fmt: []const u8, args: anytype) Error {
        const message = try u.formatToString(self.arena.allocator(), fmt, args);
        self.error_info = ErrorInfo{
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
        while (true) {
            self.allowSpace();
            if (self.peekToken() == .EOF) break;
            try self.rules.append(try self.parseRule());
            self.allowSpace();
        }
        return Program{ .rules = self.rules.items };
    }

    fn parseRule(self: *Parser) !Rule {
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

    fn parseClause(self: *Parser) !Clause {
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

    fn parseArg(self: *Parser) !Arg {
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
    error_info: ?ErrorInfo,

    pub const Error = error{
        // sets error_info
        PlannerError,

        // does not set error_info
        OutOfMemory,
    };

    pub const ErrorInfo = struct {
        rule_ix: usize,
        message: []const u8,
        pub const format = u.formatViaDump;
    };

    fn setError(self: *Planner, rule_ix: usize, comptime fmt: []const u8, args: anytype) Error {
        const message = try u.formatToString(self.arena.allocator(), fmt, args);
        self.error_info = ErrorInfo{
            .rule_ix = rule_ix,
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
        for (program.rules) |rule, rule_ix|
            try rules.append(try self.planRule(rule, rule_ix));
        return ProgramPlan{ .rules = rules.toOwnedSlice() };
    }

    fn planRule(self: *Planner, rule: Rule, rule_ix: usize) !RulePlan {
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
                        return self.setError(rule_ix, "Variable {s} is used in head but not bound in body", .{variable});
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

// ---

const Database = struct {
    sets: u.DeepHashMap(Name, Set),

    pub const format = u.formatViaDump;

    pub fn dumpInto(self: Database, writer: anytype, indent: u32) u.WriterError(@TypeOf(writer))!void {
        const entries = u.deepSortHashMap(self.sets);
        defer u.sorting_allocator.free(entries);
        for (entries) |entry| {
            try entry.value.dumpIntoWithName(writer, indent, entry.key);
        }
    }
};

const Set = struct {
    rows: u.DeepHashSet(Row),

    pub fn init(allocator: u.Allocator) Set {
        return Set{ .rows = u.DeepHashSet(Row).init(allocator) };
    }

    pub fn insert(self: *Set, row: Row) !void {
        try self.rows.put(row, {});
    }

    pub fn iterator(self: Set) Iterator {
        return Iterator{ .iter = self.rows.iterator() };
    }

    pub const Iterator = struct {
        iter: u.DeepHashSet(Row).Iterator,

        pub fn next(self: *Iterator) ?Row {
            if (self.iter.next()) |entry| {
                return entry.key_ptr.*;
            } else {
                return null;
            }
        }
    };

    pub const format = u.formatViaDump;

    pub fn dumpInto(self: Set, writer: anytype, indent: u32) u.WriterError(@TypeOf(writer))!void {
        self.dumpIntoWithName(writer, indent, "");
    }

    pub fn dumpIntoWithName(self: Set, writer: anytype, indent: u32, name: []const u8) u.WriterError(@TypeOf(writer))!void {
        _ = indent;
        const entries = u.deepSortHashMap(self.rows);
        defer u.sorting_allocator.free(entries);
        for (entries) |entry| {
            try std.fmt.format(writer, "{s}(", .{name});
            for (entry.key) |atom, i| {
                if (i != 0) try writer.writeAll(", ");
                try std.fmt.format(writer, "{}", .{atom});
            }
            try writer.writeAll(").\n");
        }
    }
};

pub const Row = []const Atom;

const Interpreter = struct {
    arena: *u.ArenaAllocator,
    sets: u.DeepHashMap(Name, Set),
    error_info: ?ErrorInfo,

    pub const Error = error{
        // sets error_info
        InterpreterError,

        // does not set error_info
        OutOfMemory,
    };

    pub const ErrorInfo = struct {
        rule_ix: usize,
        message: []const u8,
        pub const format = u.formatViaDump;
    };

    fn setError(self: *Planner, rule_ix: usize, comptime fmt: []const u8, args: anytype) Error {
        const message = try u.formatToString(self.arena.allocator(), fmt, args);
        self.error_info = ErrorInfo{
            .rule_ix = rule_ix,
            .message = message,
        };
        return error.PlannerError;
    }

    pub fn getSet(self: *Interpreter, set_name: Name) !*Set {
        const set_entry = try self.sets.getOrPut(set_name);
        if (!set_entry.found_existing)
            set_entry.value_ptr.* = Set.init(self.arena.allocator());
        return set_entry.value_ptr;
    }

    pub fn interpretProgramPlan(self: *Interpreter, program_plan: ProgramPlan) !Database {
        while (true) {
            var changed = false;
            for (program_plan.rules) |rule_plan, rule_ix| {
                const old_set = try self.getSet(rule_plan.set_name);
                const new_set = try self.interpretPlanExpr(rule_plan.body, rule_ix);
                var new_set_iter = new_set.iterator();
                while (new_set_iter.next()) |row| {
                    const entry = try old_set.rows.getOrPut(row);
                    if (!entry.found_existing)
                        changed = true;
                }
            }
            if (!changed) break;
        }
        return Database{ .sets = self.sets };
    }

    pub fn interpretPlanExpr(self: *Interpreter, plan_expr: PlanExpr, rule_ix: usize) Error!Set {
        var set = Set.init(self.arena.allocator());
        switch (plan_expr) {
            .Row => |row| {
                try set.insert(row.atoms);
            },
            .Scan => |scan| {
                const scan_set = try self.getSet(scan.set_name);
                var scan_set_iter = scan_set.iterator();
                while (scan_set_iter.next()) |in_row|
                    if (in_row.len == scan.num_columns)
                        try set.insert(in_row);
            },
            .FilterConstant => |filter_constant| {
                const in = try self.interpretPlanExpr(filter_constant.in.*, rule_ix);
                var in_iter = in.iterator();
                while (in_iter.next()) |in_row|
                    if (u.deepEqual(in_row[filter_constant.column], filter_constant.constant))
                        try set.insert(in_row);
            },
            .FilterEqual => |filter_equal| {
                const in = try self.interpretPlanExpr(filter_equal.in.*, rule_ix);
                var in_iter = in.iterator();
                while (in_iter.next()) |in_row|
                    if (u.deepEqual(
                        in_row[filter_equal.columns[0]],
                        in_row[filter_equal.columns[1]],
                    ))
                        try set.insert(in_row);
            },
            .Product => |product| {
                const in0 = try self.interpretPlanExpr(product.in[0].*, rule_ix);
                const in1 = try self.interpretPlanExpr(product.in[1].*, rule_ix);
                var in0_iter = in0.iterator();
                while (in0_iter.next()) |in_row0| {
                    var in1_iter = in1.iterator();
                    while (in1_iter.next()) |in_row1| {
                        const out_row = try std.mem.concat(self.arena.allocator(), Atom, &.{ in_row0, in_row1 });
                        try set.insert(out_row);
                    }
                }
            },
            .Project => |project| {
                const in = try self.interpretPlanExpr(project.in.*, rule_ix);
                var in_iter = in.iterator();
                while (in_iter.next()) |in_row| {
                    const out_row = try self.arena.allocator().alloc(Atom, project.columns.len);
                    for (project.columns) |column, i| {
                        out_row[i] = in_row[column];
                    }
                    try set.insert(out_row);
                }
            },
        }
        return set;
    }
};

// ---

pub const Runner = struct {
    arena: *u.ArenaAllocator,

    tokenizer: ?Tokenizer = null,
    parser: ?Parser = null,
    planner: ?Planner = null,
    interpreter: ?Interpreter = null,

    pub fn run(self: *Runner, source: []const u8) !void {
        self.tokenizer = Tokenizer{
            .arena = self.arena,
            .source = source,
            .position = 0,
            .tokens = u.ArrayList(Token).init(self.arena.allocator()),
            .error_info = null,
        };
        const tokens = try self.tokenizer.?.tokenize();

        self.parser = Parser{
            .arena = self.arena,
            .tokens = tokens,
            .position = 0,
            .rules = u.ArrayList(Rule).init(self.arena.allocator()),
            .error_info = null,
        };
        const program = try self.parser.?.parseProgram();

        self.planner = Planner{
            .arena = self.arena,
            .error_info = null,
        };
        const program_plan = try self.planner.?.planProgram(program);

        self.interpreter = Interpreter{
            .arena = self.arena,
            .sets = u.DeepHashMap(Name, Set).init(self.arena.allocator()),
            .error_info = null,
        };
        _ = try self.interpreter.?.interpretProgramPlan(program_plan);
    }

    pub fn printed(self: *Runner, run_result: @typeInfo(@TypeOf(run)).Fn.return_type.?) ![]const u8 {
        if (run_result) {
            // TODO inlining this variable causes a surprising segfault
            const database = Database{ .sets = self.interpreter.?.sets };
            return u.formatToString(self.arena.allocator(), "{}", .{
                database,
            });
        } else |err| switch (err) {
            error.TokenizerError => return u.formatToString(self.arena.allocator(), "{}:\n{}", .{ err, self.tokenizer.?.error_info.? }),
            error.ParserError => return u.formatToString(self.arena.allocator(), "{}:\n{}", .{ err, self.parser.?.error_info.? }),
            error.PlannerError => return u.formatToString(self.arena.allocator(), "{}:\n{}", .{ err, self.planner.?.error_info.? }),
            error.InterpreterError => return u.formatToString(self.arena.allocator(), "{}:\n{}", .{ err, self.interpreter.?.error_info.? }),
            error.OutOfMemory => return u.formatToString(self.arena.allocator(), "{}", .{err}),
        }
    }
};

// ---

fn testEndToEnd(source: []const u8, expected: []const u8) !void {
    var arena = u.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var runner = Runner{ .arena = &arena };
    const found = try runner.printed(runner.run(source));
    try std.testing.expectEqualStrings(expected, std.mem.trim(u8, found, "\n "));
}

test "end to end" {
    const cases = @embedFile("../test/end_to_end.test");
    var case_iter = std.mem.split(u8, cases, "\n\n");
    while (case_iter.next()) |case| {
        const sep = "---";
        const source_end = std.mem.indexOf(u8, case, sep).?;
        const source = case[0..source_end];
        const expected = std.mem.trim(u8, case[source_end + sep.len ..], "\n ");
        try testEndToEnd(source, expected);
    }
}
