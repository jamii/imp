usingnamespace @import("common.zig");

const Store = @import("./store.zig").Store;
const syntax = @import("./syntax.zig");

// expr =
//   "(" expr ")"
//   "none"
//   "some"
//   number
//   string
//   name
//   "when" expr "then" expr
//   "\" arg+ "->" expr
//   "[" expr "]"
//   "#" name expr
//   "!" expr
//   "if" expr "then" expr "else" expr
//   "let" name "=" expr "in" expr
//   "//" ^"\n" "\n"
//   expr expr
//   expr binop expr
//   expr ":" name

// name =
//   alpha (alpha | digit | "_")*

// arg =
//   name
//   "[" name "]"

// binop =
//   "|"
//   "&"
//   "."
//   "="
//   "+"
//   "-"
//   "*"
//   "/"
//   ">"
//   ">="
//   "<"
//   "<="

// TODO https://github.com/ziglang/zig/issues/2647
pub fn parse(store: *Store, source: []const u8, error_info: *ErrorInfo) Error ! *const syntax.Expr {
    var parser = Parser{
        .store = store,
        .source = source,
        .position = 0,
        .error_info = error_info,
    };
    const expr = try parser.parseExpr();
    _ = try parser.expect(.EOF);
    return expr;
}

pub const Error = error {
    Error,
    OutOfMemory,
    Utf8InvalidStartByte,
    InvalidUtf8,
    InvalidCharacter,
}
|| @TypeOf(std.unicode.utf8Decode).ReturnType.ErrorSet;

pub const ErrorInfo = struct {
    start: usize,
    end: usize,
    message: []const u8,

    pub fn init() ErrorInfo {
        return ErrorInfo{
            .start = 0,
            .end = 0,
            .message = "not an error",
        };
    }
};

// --------------------------------------------------------------------------------

const Token = union(enum) {
    // core
    OpenGroup, CloseGroup,
    None,
    Some,
    Number: f64,
    String: []const u8, // valid utf8
    Union,
    Intersect,
    Product,
    Equal,
    Name: []const u8, // ascii, non-empty
    When, Then,
    AbstractArgs, AbstractBody,
    OpenBox, CloseBox,
    Annotate,

    // sugar
    Negate,
    If, Else,
    Let, In,
    Lookup,

    // natives
    Add,
    Subtract,
    Multiply,
    Divide,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,

    // not an actual token but used for precedence
    Apply,

    // not matched by anything but used to check that we parsed everything
    EOF,

    fn bindsTighterThan(self: Token, other: Token) bool {
        return switch (self) {
            .Lookup => (other != .Lookup),
            .Product => (other == .Union),
            .Multiply, .Divide => (other == .Add) or (other == .Subtract),
            else => false,
        };
    }
};

fn appendChar(bytes: *ArrayList(u8), char: u21) !void {
    var char_bytes = [4]u8{0,0,0,0};
    const len = std.unicode.utf8Encode(char, &char_bytes)
        // we got this char from utf8Decode so it must be legit
        catch unreachable;
    try bytes.appendSlice(char_bytes[0..len]);
}

const Parser = struct {
    store: *Store,
    source: []const u8,
    position: usize,
    error_info: *ErrorInfo,

    fn putApplyOp(self: *Parser, name: syntax.Name, left: *const syntax.Expr, right: *const syntax.Expr, start: usize, end: usize) ! *const syntax.Expr {
        const expr1 = try self.store.putSyntax(.{.Name=name}, start, end);
        const expr2 = try self.store.putSyntax(.{.Apply=.{.left=expr1, .right=left}}, start, end);
        const expr3 = try self.store.putSyntax(.{.Apply=.{.left=expr2, .right=right}}, start, end);
        return expr3;
    }

    fn setError(self: *Parser, start: usize, comptime fmt: []const u8, args: var) Error {
        self.error_info.start = start;
        self.error_info.end = self.position;
        if (format(&self.store.arena.allocator, fmt, args)) |message| {
            self.error_info.message = message;
            return error.Error;
        } else |err| {
            switch (err) {
                error.OutOfMemory => self.error_info.message = "out of memory",
            }
            return err;
        }
    }

    // use this instead of std.unicode.Utf8Iterator because we want to return the position of any unicode error
    fn nextUtf8Char(self: *Parser) ! ?u21 {
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

    fn nextAsciiChar(self: *Parser) ! ?u8 {
        const start = self.position;
        if (try self.nextUtf8Char()) |char| {
            const ascii_char = @intCast(u8, char & 0b0111_1111);
            if (@as(u21, char) != ascii_char) {
                return self.setError(start, "unicode characters may only appear in strings and comments", .{});
            }
            return ascii_char;
        } else {
            return null;
        }
    }

    // called after seeing '"'
    fn tokenizeString(self: *Parser) ! []const u8 {
        var bytes = ArrayList(u8).init(&self.store.arena.allocator);
        const string_start = self.position - 1;
        while (true) {
            const char_start = self.position;
            if (try self.nextUtf8Char()) |char| {
                switch (char) {
                    '\\' => {
                        if (try self.nextUtf8Char()) |escaped_char| {
                            switch (escaped_char) {
                                '"' => try bytes.append('"'),
                                'n' => try bytes.append('\n'),
                                else => return self.setError(char_start, "invalid string escape", .{}),
                            }
                        } else {
                            return self.setError(char_start, "unfinished string escape", .{});
                        }

                    },
                    '"' => {
                        break;
                    },
                    else => {
                        try appendChar(&bytes, char);
                    },
                }
            } else {
                return self.setError(string_start, "unfinished string", .{});
            }
        }
        return bytes.items;
    }

    // called after seeing "//"
    fn tokenizeComment(self: *Parser) ! void {
        while (true) {
            // utf8 chars are allowed in comments
            if (try self.nextUtf8Char()) |char2| {
                if (char2 == '\n') {
                    break;
                }
            } else {
                break;
            }
        }
    }

    // called after seeing first char of name
    // name = alpha (alpha | digit | "_")*
    fn tokenizeName(self: *Parser) ! []const u8 {
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
    fn tokenizeNumber(self: *Parser) ! f64 {
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
            // otherwise `1.foo` can parse as `1 . foo` or `1.0 foo`
            return self.setError(start, "there must be at least one digit after a decimal point", .{});
        } else {
            return std.fmt.parseFloat(f64, self.source[start..self.position]);
        }
    }

    fn nextToken(self: *Parser) ! Token {
        const start = self.position;
        if (try self.nextAsciiChar()) |char1| {
            switch (char1) {
                '(' => return Token{.OpenGroup={}},
                ')' => return Token{.CloseGroup={}},
                '"' => return Token{.String = try self.tokenizeString()},
                '|' => return Token{.Union={}},
                '&' => return Token{.Intersect={}},
                '.' => return Token{.Product={}},
                '=' => return Token{.Equal={}},
                '\\' => return Token{.AbstractArgs={}},
                '-' => {
                    const position = self.position;
                    if ((try self.nextAsciiChar()) orelse 0 == '>') {
                        return Token{.AbstractBody={}};
                    } else {
                        self.position = position;
                        return Token{.Subtract={}};
                    }
                },
                '[' => return Token{.OpenBox={}},
                ']' => return Token{.CloseBox={}},
                '#' => return Token{.Annotate={}},
                '!' => return Token{.Negate={}},
                ':' => return Token{.Lookup={}},
                '+' => return Token{.Add={}},
                '*' => return Token{.Multiply={}},
                '/' => {
                    const position = self.position;
                    if ((try self.nextAsciiChar()) orelse 0 == '/') {
                        try self.tokenizeComment();
                        return self.nextToken();
                    } else {
                        self.position = position;
                        return Token{.Divide={}};
                    }
                },
                '<' => {
                    const position = self.position;
                    if ((try self.nextAsciiChar()) orelse 0 == '=') {
                        return Token{.LessThanOrEqual={}};
                    } else {
                        self.position = position;
                        return Token{.LessThan={}};
                    }
                },
                '>' => {
                    const position = self.position;
                    if ((try self.nextAsciiChar()) orelse 0 == '=') {
                        return Token{.GreaterThanOrEqual={}};
                    } else {
                        self.position = position;
                        return Token{.GreaterThan={}};
                    }
                },
                else => {
                    const ascii_char1 = @intCast(u8, char1 & 0b0111_1111);
                    if (char1 != @as(u21, ascii_char1)) {
                        return self.setError(start, "unicode characters may only appear in strings and comments", .{});
                    } else if (std.ascii.isSpace(ascii_char1)) {
                        return self.nextToken();
                    } else if (std.ascii.isDigit(ascii_char1)) {
                        return Token{.Number = try self.tokenizeNumber()};
                    } else if (std.ascii.isAlpha(ascii_char1)) {
                        const name = try self.tokenizeName();
                        if (meta.deepEqual(name, "none")) return Token{.None={}};
                        if (meta.deepEqual(name, "some")) return Token{.Some={}};
                        if (meta.deepEqual(name, "when")) return Token{.When={}};
                        if (meta.deepEqual(name, "then")) return Token{.Then={}};
                        if (meta.deepEqual(name, "if")) return Token{.If={}};
                        if (meta.deepEqual(name, "else")) return Token{.Else={}};
                        if (meta.deepEqual(name, "let")) return Token{.Let={}};
                        if (meta.deepEqual(name, "in")) return Token{.In={}};
                        return Token{.Name = name};
                    } else {
                        return self.setError(start, "invalid token", .{});
                    }
                }
            }
        } else {
            return Token{.EOF={}};
        }
    }

    fn expect(self: *Parser, expected: @TagType(Token)) ! Token {
        const start = self.position;
        const found = try self.nextToken();
        if (std.meta.activeTag(found) != expected) {
            return self.setError(start, "Expected {}, found {}", .{expected, found});
        } else {
            return found;
        }
    }

    // returns null if this isn't the start of an expression
    fn parseExprInnerMaybe(self: *Parser) Error ! ?*const syntax.Expr {
        const start = self.position;
        const token = try self.nextToken();
        switch (token) {
            // "(" expr ")"
            .OpenGroup => {
                const expr = try self.parseExpr();
                _ = try self.expect(.CloseGroup);
                return expr;
            },
            // "none"
            .None => return self.store.putSyntax(.None, start, self.position),
            // "some"
            .Some => return self.store.putSyntax(.Some, start, self.position),
            // number
            .Number => |number| return self.store.putSyntax(.{.Scalar=.{.Number=number}}, start, self.position),
            // string
            .String => |string| return self.store.putSyntax(.{.Scalar=.{.String=string}}, start, self.position),
            // name
            .Name => |name| return self.store.putSyntax(.{.Name=name}, start, self.position),
            // "when" expr "then" expr
            .When => {
                const condition = try self.parseExpr();
                _ = try self.expect(.Then);
                const true_branch = try self.parseExpr();
                return self.store.putSyntax(.{.When=.{.condition=condition,.true_branch=true_branch}}, start, self.position);
            },
            // "\" arg+ "->" expr
            .AbstractArgs => {
                var args = ArrayList(syntax.Arg).init(&self.store.arena.allocator);
                while (true) {
                    const arg_start = self.position;
                    const arg_token = try self.nextToken();
                    // arg = name | "[" name "]"
                    switch (arg_token) {
                        .Name => |name| try args.append(.{.name=name, .unbox=false}),
                        .OpenBox => {
                            const name = (try self.expect(.Name)).Name;
                            try args.append(.{.name=name, .unbox=true});
                            _ = try self.expect(.CloseBox);
                        },
                        .AbstractBody => break,
                        else => return self.setError(arg_start, "Expected name or [ or ->, found {}", .{arg_token}),
                    }
                }
                if (args.items.len == 0) {
                    return self.setError(start, "Abstract must have at least one arg", .{});
                }
                const body = try self.parseExpr();
                return self.store.putSyntax(.{.Abstract=.{.args=args.items, .body=body}}, start, self.position);
            },
            // "[" expr "]"
            .OpenBox => {
                const expr = try self.parseExpr();
                _ = try self.expect(.CloseBox);
                return self.store.putSyntax(.{.Box=expr}, start, self.position);
            },
            // "#" name expr
            .Annotate => {
                const annotation = (try self.expect(.Name)).Name;
                const body = try self.parseExpr();
                return self.store.putSyntax(.{.Annotate=.{.annotation=annotation, .body=body}}, start, self.position);
            },
            // "!" expr
            .Negate => {
                const expr = try self.parseExpr();
                return self.store.putSyntax(.{.Negate=expr}, start, self.position);
            },
            // "if" expr "then" expr "else" expr
            .If => {
                const condition = try self.parseExpr();
                _ = try self.expect(.Then);
                const true_branch = try self.parseExpr();
                _ = try self.expect(.Else);
                const false_branch = try self.parseExpr();
                return self.store.putSyntax(.{.If=.{.condition=condition, .true_branch=true_branch, .false_branch=false_branch}}, start, self.position);
            },
            // "let" name "=" expr "in" expr
            .Let => {
                const name = (try self.expect(.Name)).Name;
                _ = try self.expect(.Equal);
                const value = try self.parseExpr();
                _ = try self.expect(.In);
                const body = try self.parseExpr();
                return self.store.putSyntax(.{.Let=.{.name=name, .value=value, .body=body}}, start, self.position);
            },
            // otherwise not an expression but might be rhs of apply or binop so don't error yet
            else => {
                self.position = start;
                return null;
            },
        }
    }

    // when parsing initial expr there can't be an apply or binop so go ahead and error
    fn parseExprInner(self: *Parser) Error ! *const syntax.Expr {
        if (try self.parseExprInnerMaybe()) |expr| {
            return expr;
        } else {
            const start = self.position;
            const token = try self.nextToken();
            return self.setError(start, "Expected start of expression, found {}", .{token});
        }
    }

    // we're looking at:
    //   prev_expr prev_op left op right
    // possible cases are:
    //   * prev_op==null or op.bindsTighterThan(prev_op):
    //     prev_expr prev_op (left op right)
    //   * prev_op==op or prev_op.bindsTighterThan(op):
    //     (prev_expr prev_op left) op right
    //   * otherwise:
    //     parse_error "ambiguous precedence"
    fn parseExprOuter(self: *Parser, prev_op: ?Token) Error ! *const syntax.Expr {
        var left = try self.parseExprInner();
        while (true) {
            const op_start = self.position;
            const op = try self.nextToken();
            switch (op) {
                // expr binop expr
                .Union, .Intersect, .Product, .Equal, .Add, .Subtract, .Multiply, .Divide, .LessThan, .LessThanOrEqual, .GreaterThan, .GreaterThanOrEqual => {
                    if (prev_op == null or op.bindsTighterThan(prev_op.?)) {
                        const right = try self.parseExprOuter(op);
                        left = try switch (op) {
                            // core ops
                            .Union => self.store.putSyntax(.{.Union = .{.left=left, .right=right}}, op_start, self.position),
                            .Intersect => self.store.putSyntax(.{.Intersect = .{.left=left, .right=right}}, op_start, self.position),
                            .Product => self.store.putSyntax(.{.Product = .{.left=left, .right=right}}, op_start, self.position),
                            .Equal => self.store.putSyntax(.{.Equal = .{.left=left, .right=right}}, op_start, self.position),

                            // native functions
                            .Add => self.putApplyOp("+", left, right, op_start, self.position),
                            .Subtract => self.putApplyOp("-", left, right, op_start, self.position),
                            .Multiply => self.putApplyOp("*", left, right, op_start, self.position),
                            .Divide => self.putApplyOp("/", left, right, op_start, self.position),
                            .LessThan => self.putApplyOp("<", left, right, op_start, self.position),
                            .LessThanOrEqual => self.putApplyOp("<=", left, right, op_start, self.position),
                            .GreaterThan => self.putApplyOp(">", left, right, op_start, self.position),
                            .GreaterThanOrEqual => self.putApplyOp(">=", left, right, op_start, self.position),

                            else => unreachable,
                        };
                    } else if (tagEqual(prev_op.?, op) or prev_op.?.bindsTighterThan(op)) {
                        self.position = op_start;
                        return left;
                    } else {
                        return self.setError(op_start, "Ambiguous precedence for {} vs {}", .{prev_op.?, op});
                    }
                },

                // expr ":" name
                .Lookup => {
                    if (prev_op == null or op.bindsTighterThan(prev_op.?)) {
                        const name = (try self.expect(.Name)).Name;
                        left = try self.store.putSyntax(.{.Lookup = .{.value = left, .name = name}}, op_start, self.position);
                    } else if (tagEqual(prev_op.?, op) or prev_op.?.bindsTighterThan(op)) {
                        self.position = op_start;
                        return left;
                    } else {
                        return self.setError(op_start, "Ambiguous precedence for {} vs {}", .{prev_op.?, op});
                    }
                },

                // not a binop, might be an apply
                else => {
                    self.position = op_start;

                    // expr expr
                    if (try self.parseExprInnerMaybe()) |right| {
                        const apply: Token = .Apply;
                        if (prev_op == null or apply.bindsTighterThan(prev_op.?)) {
                            left = try self.store.putSyntax(.{.Apply=.{.left=left, .right=right}}, op_start, self.position);
                        } else if (tagEqual(prev_op.?, apply) or prev_op.?.bindsTighterThan(apply)) {
                            self.position = op_start;
                            return left;
                        } else {
                            return self.setError(op_start, "Ambiguous precedence for {} vs {}", .{prev_op.?, apply});
                        }
                    } else {
                        // no more binary things to apply
                        self.position = op_start;
                        return left;
                    }
                },
            }
        }
    }

    fn parseExpr(self: *Parser) Error ! *const syntax.Expr {
        return self.parseExprOuter(null);
    }
};

test "binding partial order" {
    var tokens = ArrayList(Token).init(std.testing.allocator);
    defer tokens.deinit();

    // generate examples of all posible tokens
    inline for (@typeInfo(Token).Union.fields) |fti| {
       // leave value undefined to test that bindsTighterThan doesn't access it
        try tokens.append(@unionInit(Token, fti.name, undefined));
    }

    for (tokens.items) |token1| {
        // non-reflexive
        expect(!token1.bindsTighterThan(token1));
        for (tokens.items) |token2| {
            // non-symmetric
            if (token1.bindsTighterThan(token2)) {
                expect(!token2.bindsTighterThan(token1));
            }
            for (tokens.items) |token3| {
                // transitive
                if (token1.bindsTighterThan(token2) and token2.bindsTighterThan(token3)) {
                    expect(token1.bindsTighterThan(token3));
                }
            }
        }
    }
}

fn testParse(source: []const u8, expected: []const u8) !void {
    var arena = ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var store = Store.init(&arena);
    var error_info = ErrorInfo.init();
    if (parse(&store, source, &error_info)) |found| {
        var bytes = ArrayList(u8).init(std.testing.allocator);
        defer bytes.deinit();
        try found.dumpInto(bytes.outStream(), 0);
        if (!meta.deepEqual(expected, bytes.items)) {
            panic("\nExpected parse:\n{}\n\nFound parse:\n{}", .{expected, bytes.items});
        }
    } else |err| {
        warn("\nExpected parse:\n{}\n\nFound error:\n{}\n", .{expected, error_info.message});
        return err;
    }
}

fn testError(source: []const u8, expected: []const u8) !void {
    var arena = ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var store = Store.init(&arena);
    var error_info = ErrorInfo.init();
    if (parse(&store, source, &error_info)) |found| {
        var bytes = ArrayList(u8).init(std.testing.allocator);
        try found.dumpInto(bytes.outStream(), 0);
        panic("\nExpected error:\n{}\n\nFound parse:\n{}", .{expected, bytes.items});
    } else |err| {
        if (!meta.deepEqual(expected, error_info.message)) {
            warn("\nExpected error:\n{}\n\nFound error:\n{}\n", .{expected, error_info.message});
            return err;
        }
    }
}

test "parse" {
    try testParse(
        \\1.3 + "foo\"bar"
            ,
            \\apply
            \\  apply
            \\    +
            \\    1.3
            \\  "foo"bar"
    );

    try testParse(
        \\a . b | c
            ,
            \\|
            \\  .
            \\    a
            \\    b
            \\  c
    );

    try testParse(
        \\a | b . c
            ,
            \\|
            \\  a
            \\  .
            \\    b
            \\    c
    );

    try testParse(
        \\a + b * c
            ,
            \\apply
            \\  apply
            \\    +
            \\    a
            \\  apply
            \\    apply
            \\      *
            \\      b
            \\    c
    );

     try testParse(
        \\a * b + c
             ,
             \\apply
             \\  apply
             \\    +
             \\    apply
             \\      apply
             \\        *
             \\        a
             \\      b
             \\  c
    );

    try testParse(
        \\!a:b
            ,
            \\!
            \\  : b
            \\    a
    );

    try testError(
        \\a & b | c
            ,
        \\Ambiguous precedence for Token{ .Intersect = void } vs Token{ .Union = void }
    );

    try testError(
        \\a * b / c
            ,
        \\Ambiguous precedence for Token{ .Multiply = void } vs Token{ .Divide = void }
    );

    try testError(
        \\a | b c
            ,
        \\Ambiguous precedence for Token{ .Union = void } vs Token{ .Apply = void }
    );

    try testError(
        \\a + b | c
            ,
        \\Ambiguous precedence for Token{ .Add = void } vs Token{ .Union = void }
    );

    try testParse(
        \\a | (b c):d
            ,
            \\|
            \\  a
            \\  : d
            \\    apply
            \\      b
            \\      c
    );

    try testParse(
        \\\ a [b] c d -> a . b . [c . d]
            ,
            \\\ a [b] c d ->
            \\  .
            \\    .
            \\      a
            \\      b
            \\    []
            \\      .
            \\        c
            \\        d
    );

    try testParse(
        \\let a = b in a
            ,
            \\let a =
            \\  b
            \\  a
    );

     try testParse(
        \\let a = 1 in a
            ,
            \\let a =
            \\  1
            \\  a
    );
}
