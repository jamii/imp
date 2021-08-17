const imp = @import("../../../imp.zig");
usingnamespace imp.common;
const meta = imp.meta;
const Store = imp.lang.Store;
const syntax = imp.lang.repr.syntax;

// expr =
//   expr_inner
//   expr_inner expr
//   expr_inner binop expr
//   "//" ^"\n" "\n"

// expr_inner =
//   "(" expr ")"
//   "none"
//   "some"
//   number
//   text
//   name
//   "!" expr_inner
//   "when" expr_inner expr_inner
//   "?" arg "," expr
//   "[" expr "]"
//   "fix" expr_inner expr_inner
//   "reduce" expr_inner expr_inner expr_inner
//   "enumerate" expr_inner
//   "#" name expr_inner
//   "if" expr_inner expr_inner expr_inner
//   "let" name "=" expr "in" expr
//   expr_inner ":" name

// name =
//   alpha (alpha | digit | "_")*

// arg =
//   name
//   "[" name "]"

// binop =
//   "|"
//   "&"
//   ","
//   "="
//   "+"
//   "-"
//   "*"
//   "/"
//   "%"
//   ">"
//   ">="
//   "<"
//   "<="

// TODO https://github.com/ziglang/zig/issues/2647
pub fn parse(store: *Store, source: []const u8, error_info: *?ErrorInfo) Error!*const syntax.Expr {
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

// TODO clean up this errorset
pub const Error = error{
    // sets error_info
    ParseError,

    // nothing else sets error_info
    OutOfMemory,
    Utf8InvalidStartByte,
    InvalidUtf8,
    InvalidCharacter,
    Utf8ExpectedContinuation,
    Utf8OverlongEncoding,
    Utf8EncodesSurrogateHalf,
    Utf8CodepointTooLarge,
};

pub const ErrorInfo = struct {
    start: usize,
    end: usize,
    message: []const u8,
};

// --------------------------------------------------------------------------------

const TokenTag = enum {
    // core
    OpenGroup,
    CloseGroup,
    None,
    Some,
    Number,
    Text,
    Union,
    Intersect,
    Product,
    Equal,
    Name,
    When,
    Arg,
    OpenBox,
    CloseBox,
    Fix,
    Reduce,
    Enumerate,
    Annotate,

    // sugar
    Negate,
    If,
    Let,
    In,
    Lookup,

    // natives
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulus,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,

    // not an actual token but used for precedence
    Apply,

    // not matched by anything but used to check that we parsed everything
    EOF,

    pub fn format(self: TokenTag, comptime fmt: []const u8, options: std.fmt.FormatOptions, out_stream: anytype) !void {
        try out_stream.writeAll(switch (self) {
            .OpenGroup => "`(`",
            .CloseGroup => "`)`",
            .None => "`none`",
            .Some => "`some`",
            .Number => "number",
            .Text => "text",
            .Union => "`|`",
            .Intersect => "`&`",
            .Product => "`,`",
            .Equal => "`=`",
            .Name => "name",
            .When => "`when`",
            .Arg => "`?`",
            .OpenBox => "`[`",
            .CloseBox => "`]`",
            .Fix => "`fix`",
            .Reduce => "`reduce`",
            .Enumerate => "`enumerate`",
            .Annotate => "`#`",

            .Negate => "`!`",
            .If => "`if`",
            .Let => "`let`",
            .In => "`in`",
            .Lookup => "`:`",

            .Add => "`+`",
            .Subtract => "`-`",
            .Multiply => "`*`",
            .Divide => "`/`",
            .Modulus => "%",
            .LessThan => "`<`",
            .LessThanOrEqual => "`<=`",
            .GreaterThan => "`>`",
            .GreaterThanOrEqual => "`>=`",

            .Apply => "` `",

            .EOF => "end of file",
        });
    }
};

const Token = union(TokenTag) {
    // core
    OpenGroup,
    CloseGroup,
    None,
    Some,
    Number: f64,
    Text: []const u8, // valid utf8
    Union,
    Intersect,
    Product,
    Equal,
    Name: []const u8, // ascii, non-empty
    When,
    Arg,
    OpenBox,
    CloseBox,
    Fix,
    Reduce,
    Enumerate,
    Annotate,

    // sugar
    Negate, // TODO not currently desugared because `none` is typed as 0 arity
    If,
    Let,
    In,
    Lookup,

    // natives
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulus,
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
            .Lookup => other != .Lookup,
            // we want:
            //   ?a.?b.c == ?a.(?b.c)
            // so bindsTighterThan(.Product, .Product) must return true
            .Product => other == .Union or other == .Product,
            .Multiply, .Divide => other == .Add or other == .Subtract,
            else => false,
        };
    }

    pub fn format(self: Token, comptime fmt: []const u8, options: std.fmt.FormatOptions, out_stream: anytype) !void {
        try (std.meta.activeTag(self)).format(fmt, options, out_stream);
        switch (self) {
            .Number => |number| try std.fmt.format(out_stream, " `{d}`", .{number}),
            // TODO proper escaping
            .Text => |text| try std.fmt.format(out_stream, " `\"{s}\"`", .{text}),
            .Name => |name| try std.fmt.format(out_stream, " `{s}`", .{name}),
            else => {},
        }
    }
};

fn appendChar(bytes: *ArrayList(u8), char: u21) !void {
    var char_bytes = [4]u8{ 0, 0, 0, 0 };
    const len = std.unicode.utf8Encode(char, &char_bytes)
    // we got this char from utf8Decode so it must be legit
    catch unreachable;
    try bytes.appendSlice(char_bytes[0..len]);
}

const Parser = struct {
    store: *Store,
    source: []const u8,
    position: usize,
    error_info: *?ErrorInfo,

    fn putApplyOp(self: *Parser, name: syntax.Name, left: *const syntax.Expr, right: *const syntax.Expr, start: usize, end: usize) !*const syntax.Expr {
        const expr1 = try self.store.putSyntax(.{ .Name = name }, start, end);
        const expr2 = try self.store.putSyntax(.{ .Apply = .{ .left = expr1, .right = left } }, start, end);
        const expr3 = try self.store.putSyntax(.{ .Apply = .{ .left = expr2, .right = right } }, start, end);
        return expr3;
    }

    fn setError(self: *Parser, start: usize, comptime fmt: []const u8, args: anytype) Error {
        const message = try format(&self.store.arena.allocator, fmt, args);
        self.error_info.* = ErrorInfo{
            .start = start,
            .end = self.position,
            .message = message,
        };
        return error.ParseError;
    }

    // use this instead of std.unicode.Utf8Iterator because we want to return the position of any unicode error
    fn nextUtf8Char(self: *Parser) !?u21 {
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

    fn nextAsciiChar(self: *Parser) !?u7 {
        const start = self.position;
        if (try self.nextUtf8Char()) |char| {
            const ascii_char = @truncate(u7, char);
            if (ascii_char != char) {
                return self.setError(start, "unicode characters may only appear in texts and comments", .{});
            }
            return ascii_char;
        } else {
            return null;
        }
    }

    // called after seeing '"'
    fn tokenizeText(self: *Parser) ![]const u8 {
        var bytes = ArrayList(u8).init(&self.store.arena.allocator);
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
                    '"' => {
                        break;
                    },
                    else => {
                        try appendChar(&bytes, char);
                    },
                }
            } else {
                return self.setError(text_start, "unfinished text", .{});
            }
        }
        return bytes.items;
    }

    // called after seeing "//"
    fn tokenizeComment(self: *Parser) !void {
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
    fn tokenizeName(self: *Parser) ![]const u8 {
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
    fn tokenizeNumber(self: *Parser) !f64 {
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
            return std.fmt.parseFloat(f64, self.source[start..self.position]);
        }
    }

    // returns null for comments/whitespace so we can easily skip them
    fn nextTokenMaybe(self: *Parser) !?Token {
        const start = self.position;
        if (try self.nextAsciiChar()) |char1| {
            switch (char1) {
                '(' => return Token{ .OpenGroup = {} },
                ')' => return Token{ .CloseGroup = {} },
                '"' => return Token{ .Text = try self.tokenizeText() },
                '|' => return Token{ .Union = {} },
                '&' => return Token{ .Intersect = {} },
                ',' => return Token{ .Product = {} },
                '=' => return Token{ .Equal = {} },
                '?' => return Token{ .Arg = {} },
                '-' => return Token{ .Subtract = {} },
                '[' => return Token{ .OpenBox = {} },
                ']' => return Token{ .CloseBox = {} },
                '#' => return Token{ .Annotate = {} },
                '!' => return Token{ .Negate = {} },
                ':' => return Token{ .Lookup = {} },
                '+' => return Token{ .Add = {} },
                '*' => return Token{ .Multiply = {} },
                '/' => {
                    const position = self.position;
                    if ((try self.nextAsciiChar()) orelse 0 == '/') {
                        try self.tokenizeComment();
                        return null;
                    } else {
                        self.position = position;
                        return Token{ .Divide = {} };
                    }
                },
                '%' => return Token{ .Modulus = {} },
                '<' => {
                    const position = self.position;
                    if ((try self.nextAsciiChar()) orelse 0 == '=') {
                        return Token{ .LessThanOrEqual = {} };
                    } else {
                        self.position = position;
                        return Token{ .LessThan = {} };
                    }
                },
                '>' => {
                    const position = self.position;
                    if ((try self.nextAsciiChar()) orelse 0 == '=') {
                        return Token{ .GreaterThanOrEqual = {} };
                    } else {
                        self.position = position;
                        return Token{ .GreaterThan = {} };
                    }
                },
                '~' => return Token{ .Name = "~" },
                else => {
                    const ascii_char1 = @intCast(u8, char1 & 0b0111_1111);
                    if (char1 != @as(u21, ascii_char1)) {
                        return self.setError(start, "unicode characters may only appear in texts and comments", .{});
                    } else if (std.ascii.isSpace(ascii_char1)) {
                        return null;
                    } else if (std.ascii.isDigit(ascii_char1)) {
                        return Token{ .Number = try self.tokenizeNumber() };
                    } else if (std.ascii.isAlpha(ascii_char1)) {
                        const name = try self.tokenizeName();
                        if (meta.deepEqual(name, "none")) return Token{ .None = {} };
                        if (meta.deepEqual(name, "some")) return Token{ .Some = {} };
                        if (meta.deepEqual(name, "when")) return Token{ .When = {} };
                        if (meta.deepEqual(name, "fix")) return Token{ .Fix = {} };
                        if (meta.deepEqual(name, "reduce")) return Token{ .Reduce = {} };
                        if (meta.deepEqual(name, "enumerate")) return Token{ .Enumerate = {} };
                        if (meta.deepEqual(name, "if")) return Token{ .If = {} };
                        if (meta.deepEqual(name, "let")) return Token{ .Let = {} };
                        if (meta.deepEqual(name, "in")) return Token{ .In = {} };
                        return Token{ .Name = name };
                    } else {
                        return self.setError(start, "invalid token", .{});
                    }
                },
            }
        } else {
            return Token{ .EOF = {} };
        }
    }

    fn nextToken(self: *Parser) !Token {
        while (true) {
            if (try self.nextTokenMaybe()) |token| {
                return token;
            }
        }
    }

    fn expect(self: *Parser, expected: TokenTag) !Token {
        const start = self.position;
        const found = try self.nextToken();
        if (std.meta.activeTag(found) != expected) {
            return self.setError(start, "Expected {}, found {}", .{ expected, found });
        } else {
            return found;
        }
    }

    // returns null if this isn't the start of an expression
    fn parseExprInnerMaybe(self: *Parser) Error!?*const syntax.Expr {
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
            .Number => |number| return self.store.putSyntax(.{ .Scalar = .{ .Number = number } }, start, self.position),
            // text
            .Text => |text| return self.store.putSyntax(.{ .Scalar = .{ .Text = text } }, start, self.position),
            // name
            .Name => |name| return self.store.putSyntax(.{ .Name = name }, start, self.position),
            // "when" expr_inner expr_inner
            .When => {
                const condition = try self.parseExprInner();
                const true_branch = try self.parseExprInner();
                return self.store.putSyntax(.{ .When = .{ .condition = condition, .true_branch = true_branch } }, start, self.position);
            },
            // syntax is:
            //   expr = ... | "?" arg "," expr
            // but we parse as:
            //   expr = ... | "?" arg
            // and disambiguate later so that we can make `?arg , expr` bind exactly as tightly as `expr , expr`
            .Arg => {
                const arg_start = self.position;
                const arg_token = try self.nextToken();
                const arg = arg: {
                    switch (arg_token) {
                        .Name => |name| {
                            break :arg syntax.Arg{ .name = name, .unbox = false };
                        },
                        .OpenBox => {
                            const name = (try self.expect(.Name)).Name;
                            _ = try self.expect(.CloseBox);
                            break :arg syntax.Arg{ .name = name, .unbox = true };
                        },
                        else => return self.setError(start, "Expected ?name or ?[name], found {}", .{arg_token}),
                    }
                };
                return self.store.putSyntax(.{ .Arg = arg }, start, self.position);
            },
            // "[" expr "]"
            .OpenBox => {
                const expr = try self.parseExpr();
                _ = try self.expect(.CloseBox);
                return self.store.putSyntax(.{ .Box = expr }, start, self.position);
            },
            // "fix" expr_inner expr_inner
            .Fix => {
                const init = try self.parseExprInner();
                const next = try self.parseExprInner();
                return self.store.putSyntax(.{ .Fix = .{ .init = init, .next = next } }, start, self.position);
            },
            // "reduce" expr_inner expr_inner expr_inner
            .Reduce => {
                const input = try self.parseExprInner();
                const init = try self.parseExprInner();
                const next = try self.parseExprInner();
                return self.store.putSyntax(.{ .Reduce = .{ .input = input, .init = init, .next = next } }, start, self.position);
            },
            // "enumerate" expr_inner
            .Enumerate => {
                const body = try self.parseExprInner();
                return self.store.putSyntax(.{ .Enumerate = body }, start, self.position);
            },
            // "#" name expr_inner
            .Annotate => {
                const annotation = (try self.expect(.Name)).Name;
                const body = try self.parseExprInner();
                return self.store.putSyntax(.{ .Annotate = .{ .annotation = annotation, .body = body } }, start, self.position);
            },
            // "!" expr_inner
            .Negate => {
                const expr = try self.parseExprInner();
                return self.store.putSyntax(.{ .Negate = expr }, start, self.position);
            },
            // "if" expr_inner expr_inner expr_inner
            .If => {
                const condition = try self.parseExprInner();
                const true_branch = try self.parseExprInner();
                const false_branch = try self.parseExprInner();
                return self.store.putSyntax(.{ .If = .{ .condition = condition, .true_branch = true_branch, .false_branch = false_branch } }, start, self.position);
            },
            // "let" name "=" expr "in" expr
            .Let => {
                const name = (try self.expect(.Name)).Name;
                _ = try self.expect(.Equal);
                const value = try self.parseExpr();
                _ = try self.expect(.In);
                const body = try self.parseExpr();
                return self.store.putSyntax(.{ .Let = .{ .name = name, .value = value, .body = body } }, start, self.position);
            },
            // otherwise not an expression but might be rhs of apply or binop so don't error yet
            else => {
                self.position = start;
                return null;
            },
        }
    }

    // when parsing initial expr there can't be an apply or binop so go ahead and error
    fn parseExprInner(self: *Parser) Error!*const syntax.Expr {
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
    fn parseExprOuter(self: *Parser, prev_op: ?Token) Error!*const syntax.Expr {
        var left = try self.parseExprInner();
        while (true) {
            const op_start = self.position;
            const op = try self.nextToken();
            switch (op) {
                // expr_inner binop expr
                .Union, .Intersect, .Product, .Equal, .Add, .Subtract, .Multiply, .Divide, .Modulus, .LessThan, .LessThanOrEqual, .GreaterThan, .GreaterThanOrEqual => {
                    if (prev_op == null or op.bindsTighterThan(prev_op.?)) {
                        const right = try self.parseExprOuter(op);
                        left = try switch (op) {
                            // core ops
                            .Union => self.store.putSyntax(.{ .Union = .{ .left = left, .right = right } }, op_start, self.position),
                            .Intersect => self.store.putSyntax(.{ .Intersect = .{ .left = left, .right = right } }, op_start, self.position),
                            .Product => self.store.putSyntax(.{ .Product = .{ .left = left, .right = right } }, op_start, self.position),
                            .Equal => self.store.putSyntax(.{ .Equal = .{ .left = left, .right = right } }, op_start, self.position),

                            // native functions
                            .Add => self.putApplyOp("+", left, right, op_start, self.position),
                            .Subtract => self.putApplyOp("-", left, right, op_start, self.position),
                            .Multiply => self.putApplyOp("*", left, right, op_start, self.position),
                            .Divide => self.putApplyOp("/", left, right, op_start, self.position),
                            .Modulus => self.putApplyOp("%", left, right, op_start, self.position),
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
                        return self.setError(op_start, "Ambiguous precedence for {} vs {}", .{ prev_op.?, op });
                    }
                },

                // expr_inner ":" name
                .Lookup => {
                    if (prev_op == null or op.bindsTighterThan(prev_op.?)) {
                        const name = (try self.expect(.Name)).Name;
                        left = try self.store.putSyntax(.{ .Lookup = .{ .value = left, .name = name } }, op_start, self.position);
                    } else if (tagEqual(prev_op.?, op) or prev_op.?.bindsTighterThan(op)) {
                        self.position = op_start;
                        return left;
                    } else {
                        return self.setError(op_start, "Ambiguous precedence for {} vs {}", .{ prev_op.?, op });
                    }
                },

                // not a binop, might be an apply
                else => {
                    self.position = op_start;

                    // expr_inner expr
                    if (try self.parseExprInnerMaybe()) |right| {
                        const apply: Token = .Apply;
                        if (prev_op == null or apply.bindsTighterThan(prev_op.?)) {
                            left = try self.store.putSyntax(.{ .Apply = .{ .left = left, .right = right } }, op_start, self.position);
                        } else if (tagEqual(prev_op.?, apply) or prev_op.?.bindsTighterThan(apply)) {
                            self.position = op_start;
                            return left;
                        } else {
                            return self.setError(op_start, "Ambiguous precedence for {} vs {}", .{ prev_op.?, apply });
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

    fn parseExpr(self: *Parser) Error!*const syntax.Expr {
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
