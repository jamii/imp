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
//   expr "!"
//   "?" arg expr
//   expr "@"
//   "fix" expr_inner expr_inner
//   "reduce" expr_inner expr_inner expr_inner
//   "enumerate" expr_inner
//   "#" name expr_inner
//   expr "then" expr ("else" expr)?
//   "fix"? name ":" expr ";" expr

// name =
//   alpha (alpha | digit | "_")*

// arg =
//   name
//   "@"

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

pub const TokenTag = enum {
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
    Then,
    Arg,
    Box,
    Fix,
    Reduce,
    Enumerate,
    Annotate,

    // sugar
    Negate,
    Else,
    Def,
    EndDef,
    Extend,

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
    Swap,

    // not an actual token but used for precedence
    Apply,

    // not matched by anything but used to check that we parsed everything
    EOF,

    pub fn format(self: TokenTag, comptime fmt: []const u8, _: std.fmt.FormatOptions, out_stream: anytype) !void {
        // TODO https://github.com/ziglang/zig/issues/9220
        _ = fmt;
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
            .Then => "`then`",
            .Arg => "`?`",
            .Box => "`@`",
            .Fix => "`fix`",
            .Reduce => "`reduce`",
            .Enumerate => "`enumerate`",
            .Annotate => "`#`",

            .Negate => "`!`",
            .Else => "`else`",
            .Def => "`:`",
            .EndDef => "`;`",
            .Extend => "`.`",

            .Add => "`+`",
            .Subtract => "`-`",
            .Multiply => "`*`",
            .Divide => "`/`",
            .Modulus => "%",
            .LessThan => "`<`",
            .LessThanOrEqual => "`<=`",
            .GreaterThan => "`>`",
            .GreaterThanOrEqual => "`>=`",
            .Swap => "~",

            .Apply => "` `",

            .EOF => "end of file",
        });
    }
};

pub const Token = union(TokenTag) {
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
    Then,
    Arg,
    Box,
    Fix,
    Reduce,
    Enumerate,
    Annotate,

    // sugar
    Negate, // TODO not currently desugared because `none` is typed as 0 arity
    Else,
    Def,
    EndDef,
    Extend,

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
    Swap,

    // not an actual token but used for precedence
    Apply,

    // not matched by anything but used to check that we parsed everything
    EOF,

    const PrecedenceClass = enum {
        Apply,
        Union,
        Product,
        BinOp,
        fn bindsTighterThan(self: PrecedenceClass, other: PrecedenceClass) bool {
            return switch (self) {
                .Union => false,
                .Product => other == .Union,
                .Apply, .BinOp => other == .Union or other == .Product,
            };
        }
    };

    fn precedenceClass(self: Token) PrecedenceClass {
        return switch (self) {
            .Apply, .Extend => .Apply,
            .Union, .Intersect => .Union,
            .Product => .Product,
            else => .BinOp,
        };
    }

    fn comparePrecedence(left: Token, right: Token) enum { LeftBindsTighter, Same, RightBindsTighter, Ambiguous } {
        if (tagEqual(left, right)) return .Same;
        const left_class = left.precedenceClass();
        const right_class = right.precedenceClass();
        if (left_class == .Apply and right_class == .Apply) return .Same;
        if (left_class.bindsTighterThan(right_class)) return .LeftBindsTighter;
        if (right_class.bindsTighterThan(left_class)) return .RightBindsTighter;
        return .Ambiguous;
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

pub const Parser = struct {
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

    // using this instead of std.unicode.Utf8Iterator because we want to return the position of any unicode error
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
                    '"', '\n' => {
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
    pub fn nextTokenMaybe(self: *Parser) !?Token {
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
                '@' => return Token{ .Box = {} },
                '#' => return Token{ .Annotate = {} },
                '!' => return Token{ .Negate = {} },
                ':' => return Token{ .Def = {} },
                ';' => return Token{ .EndDef = {} },
                '.' => return Token{ .Extend = {} },
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
                '~' => return Token{ .Swap = {} },
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
                        if (meta.deepEqual(name, "then")) return Token{ .Then = {} };
                        if (meta.deepEqual(name, "fix")) return Token{ .Fix = {} };
                        if (meta.deepEqual(name, "reduce")) return Token{ .Reduce = {} };
                        if (meta.deepEqual(name, "enumerate")) return Token{ .Enumerate = {} };
                        if (meta.deepEqual(name, "else")) return Token{ .Else = {} };
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

    pub fn nextToken(self: *Parser) !Token {
        while (true) {
            if (try self.nextTokenMaybe()) |token| {
                return token;
            }
        }
    }

    pub fn consumeWhitespace(self: *Parser) !void {
        while (true) {
            const start = self.position;
            if ((try self.nextTokenMaybe()) != null) {
                self.position = start;
                break;
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

    // called after seeing name ":"
    fn parseDefBody(self: *Parser, start: usize, fix: bool, name: []const u8) Error!*const syntax.Expr {
        // (name ":") expr ";" expr
        const value = try self.parseExpr();
        const value_end = self.position;
        if ((try self.nextToken()) == .EOF) {
            // in sloppy mode, allow ending with a def
            const body = try self.store.putSyntax(.{ .Name = name }, value_end, self.position);
            return self.store.putSyntax(.{ .Def = .{ .fix = fix, .name = name, .value = value, .body = body } }, start, self.position);
        } else {
            self.position = value_end;
            _ = try self.expect(.EndDef);
            const end_def_end = self.position;
            if ((try self.nextToken()) == .EOF) {
                // in sloppy mode, allow ending with a def
                const body = try self.store.putSyntax(.{ .Name = name }, end_def_end, self.position);
                return self.store.putSyntax(.{ .Def = .{ .fix = fix, .name = name, .value = value, .body = body } }, start, self.position);
            } else {
                self.position = end_def_end;
                const body = try self.parseExpr();
                return self.store.putSyntax(.{ .Def = .{ .fix = fix, .name = name, .value = value, .body = body } }, start, self.position);
            }
        }
    }

    // returns null if this isn't the start of an expression
    fn parseExprInner(self: *Parser) Error!*const syntax.Expr {
        try self.consumeWhitespace();
        const start = self.position;
        const token = try self.nextToken();
        switch (token) {
            // "(" expr ")"
            .OpenGroup => {
                const expr = try self.parseExpr();

                // in sloppy mode, close parens
                const expr_end = self.position;
                if ((try self.nextToken()) == .EOF) {
                    return expr;
                } else {
                    self.position = expr_end;
                }

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
            .Name => |name| {
                const name_end = self.position;
                if ((try self.nextToken()) == .Def) {
                    return self.parseDefBody(start, false, name);
                } else {
                    self.position = name_end;
                    return self.store.putSyntax(.{ .Name = name }, start, self.position);
                }
            },
            // "?" arg expr
            // TODO disallow whitespace between ? and arg
            .Arg => {
                const arg_token = try self.nextToken();
                const arg = arg: {
                    switch (arg_token) {
                        .Name => |name| {
                            break :arg syntax.Arg{ .name = name, .unbox = false };
                        },
                        .Box => {
                            const name = (try self.expect(.Name)).Name;
                            break :arg syntax.Arg{ .name = name, .unbox = true };
                        },
                        else => return self.setError(start, "Expected ?name or ?[name], found ?{}", .{arg_token}),
                    }
                };
                const body = try self.parseExpr();
                return self.store.putSyntax(.{ .Abstract = .{ .arg = arg, .body = body } }, start, self.position);
            },
            .Fix => {
                const fix_end = self.position;
                const maybe_name = try self.nextToken();
                const maybe_def = try self.nextToken();
                if (maybe_name == .Name and maybe_def == .Def) {
                    // "fix" name ":" expr ";" expr
                    return self.parseDefBody(start, true, maybe_name.Name);
                } else {
                    // "fix" expr_inner expr
                    self.position = fix_end;
                    const init = try self.parseExprInner();
                    const next = try self.parseExpr();
                    return self.store.putSyntax(.{ .Fix = .{ .init = init, .next = next } }, start, self.position);
                }
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
            .Else => {
                return self.setError(start, "Found `else` without `then`", .{});
            },
            .Def => {
                return self.setError(start, "Found `:` without preceding name", .{});
            },
            // allow starting with |&, for easy editing of lists
            .Product, .Union, .Intersect => {
                return self.parseExprInner();
            },
            else => {
                return self.setError(start, "Expected start of expression, found {}", .{token});
            },
        }
    }

    // we're looking at:
    //   prev_expr prev_op left op right
    // possible cases are:
    //   * prev_op==null or prev_op.comparePrecedence(op) == .RightBindsTighter:
    //     prev_expr prev_op (left op right)
    //   * prev_op.comparePrecedence(op) == .Same or prev_op.comparePrecedence(op) == .LeftBindsTighter:
    //     (prev_expr prev_op left) op right
    //   * otherwise:
    //     parse_error "ambiguous precedence"
    fn parseExprOuter(self: *Parser, prev_op: ?Token) Error!*const syntax.Expr {
        try self.consumeWhitespace();
        const start = self.position;
        var left = try self.parseExprInner();
        while (true) {
            const op_start = self.position;
            var whitespace_before_op: bool = undefined;
            var op: Token = undefined;
            if (try self.nextTokenMaybe()) |token| {
                whitespace_before_op = false;
                op = token;
            } else {
                whitespace_before_op = true;
                op = try self.nextToken();
            }
            switch (op) {
                // expr_inner binop expr
                .Union, .Intersect, .Product, .Extend, .Equal, .Add, .Subtract, .Multiply, .Divide, .Modulus, .LessThan, .LessThanOrEqual, .GreaterThan, .GreaterThanOrEqual => {
                    const precedence = if (prev_op == null) .RightBindsTighter else prev_op.?.comparePrecedence(op);
                    switch (precedence) {
                        .RightBindsTighter => {
                            const right = try self.parseExprOuter(op);
                            left = try switch (op) {
                                // core ops
                                .Union => self.store.putSyntax(.{ .Union = .{ .left = left, .right = right } }, start, self.position),
                                .Intersect => self.store.putSyntax(.{ .Intersect = .{ .left = left, .right = right } }, start, self.position),
                                .Product => self.store.putSyntax(.{ .Product = .{ .left = left, .right = right } }, start, self.position),
                                .Equal => self.store.putSyntax(.{ .Equal = .{ .left = left, .right = right } }, start, self.position),

                                // sugar
                                .Extend => self.store.putSyntax(.{ .Extend = .{ .left = left, .right = right } }, start, self.position),

                                // native functions
                                .Add => self.putApplyOp("+", left, right, start, self.position),
                                .Subtract => self.putApplyOp("-", left, right, start, self.position),
                                .Multiply => self.putApplyOp("*", left, right, start, self.position),
                                .Divide => self.putApplyOp("/", left, right, start, self.position),
                                .Modulus => self.putApplyOp("%", left, right, start, self.position),
                                .LessThan => self.putApplyOp("<", left, right, start, self.position),
                                .LessThanOrEqual => self.putApplyOp("<=", left, right, start, self.position),
                                .GreaterThan => self.putApplyOp(">", left, right, start, self.position),
                                .GreaterThanOrEqual => self.putApplyOp(">=", left, right, start, self.position),

                                else => unreachable,
                            };
                        },
                        .LeftBindsTighter, .Same => {
                            self.position = op_start;
                            return left;
                        },
                        .Ambiguous => {
                            return self.setError(op_start, "Ambiguous precedence for {} vs {}", .{ prev_op.?, op });
                        },
                    }
                },

                .Negate => {
                    if (prev_op == null or !whitespace_before_op) {
                        left = try self.store.putSyntax(.{ .Negate = left }, start, self.position);
                    } else {
                        self.position = op_start;
                        return left;
                    }
                },

                .Box => {
                    if (prev_op == null or !whitespace_before_op) {
                        left = try self.store.putSyntax(.{ .Box = left }, start, self.position);
                    } else {
                        self.position = op_start;
                        return left;
                    }
                },

                .Swap => {
                    if (prev_op == null or !whitespace_before_op) {
                        const swap = try self.store.putSyntax(.{ .Name = "~" }, op_start, self.position);
                        left = try self.store.putSyntax(.{ .Apply = .{ .left = left, .right = swap } }, start, self.position);
                    } else {
                        self.position = op_start;
                        return left;
                    }
                },

                // expr "then" expr ("else" expr)?
                .Then => {
                    if (prev_op == null) {
                        const true_branch = try self.parseExpr();
                        const true_branch_end = self.position;
                        if ((try self.nextToken()) == .Else) {
                            const false_branch = try self.parseExpr();
                            left = try self.store.putSyntax(.{ .ThenElse = .{ .condition = left, .true_branch = true_branch, .false_branch = false_branch } }, start, self.position);
                        } else {
                            self.position = true_branch_end;
                            left = try self.store.putSyntax(.{ .Then = .{ .condition = left, .true_branch = true_branch } }, start, self.position);
                        }
                    } else {
                        self.position = op_start;
                        return left;
                    }
                },

                // we're between `then` and `else`, backtrack
                .Else => {
                    self.position = op_start;
                    return left;
                },

                .CloseGroup, .EndDef, .EOF => {
                    // no more binary things to apply
                    self.position = op_start;
                    return left;
                },

                // not a binop, might be an apply
                else => {
                    self.position = op_start;

                    // expr expr
                    const right = try self.parseExprOuter(.Apply);
                    const precedence = if (prev_op == null) .RightBindsTighter else prev_op.?.comparePrecedence(.Apply);
                    switch (precedence) {
                        .RightBindsTighter => {
                            left = try self.store.putSyntax(.{ .Apply = .{ .left = left, .right = right } }, start, self.position);
                        },
                        .LeftBindsTighter, .Same => {
                            self.position = op_start;
                            return left;
                        },
                        .Ambiguous => {
                            return self.setError(op_start, "Ambiguous precedence for {} vs {}", .{ prev_op.?, .Apply });
                        },
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
    var classes = ArrayList(Token.PrecedenceClass).init(std.testing.allocator);
    defer classes.deinit();

    // generate examples of all posible classes
    inline for (@typeInfo(Token.PrecedenceClass).Enum.fields) |fti| {
        try classes.append(@intToEnum(Token.PrecedenceClass, fti.value));
    }

    for (classes.items) |class1| {
        // non-reflexive
        try expect(!class1.bindsTighterThan(class1));
        for (classes.items) |class2| {
            // non-symmetric
            if (class1.bindsTighterThan(class2)) {
                try expect(!class2.bindsTighterThan(class1));
            }
            for (classes.items) |class3| {
                // transitive
                if (class1.bindsTighterThan(class2) and class2.bindsTighterThan(class3)) {
                    try expect(class1.bindsTighterThan(class3));
                }
            }
        }
    }
}
