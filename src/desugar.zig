usingnamespace @import("./common.zig");

const Store = @import("./store.zig").Store;
const syntax = @import("./syntax.zig");
const core = @import("./core.zig");

pub fn desugar(store: *Store, syntax_expr: *const syntax.Expr, desugar_error_info: *DesugarErrorInfo) DesugarError ! *const core.Expr {
    var desugarer = Desugarer{
        .store = store,
        .scope = ArrayList(syntax.Arg).init(&store.arena.allocator),
        .desugar_error_info = desugar_error_info,
    };
    return desugarer.desugar(syntax_expr);
}

pub const DesugarError = error {
    DesugarError,
    OutOfMemory,
};

pub const DesugarErrorInfo = struct {
    expr: ?*const syntax.Expr,
    message: []const u8,

    pub fn init() DesugarErrorInfo {
        return DesugarErrorInfo{
            .expr = null,
            .message = "not an error",
        };
    }
};

// --------------------------------------------------------------------------------

const Desugarer = struct {
    store: *Store,
    scope: ArrayList(syntax.Arg),
    desugar_error_info: *DesugarErrorInfo,

    fn desugarError(self: *Desugarer, syntax_expr: *const syntax.Expr, comptime fmt: []const u8, args: var) DesugarError {
        self.desugar_error_info.expr = syntax_expr;
        self.desugar_error_info.message = try format(&self.store.arena.allocator, fmt, args);
        return error.DesugarError;
    }

    // fn desugar_let

    fn desugar(self: *Desugarer, syntax_expr: *const syntax.Expr) DesugarError ! *const core.Expr {
        switch (syntax_expr.*) {
            .None => return try self.store.putCore(.None, syntax_expr),
            .Some => return try self.store.putCore(.Some, syntax_expr),
            .Scalar => |scalar| return try self.store.putCore(.{.Scalar = scalar}, syntax_expr),
            .Union => |pair| {
                return try self.store.putCore(
                    .{.Union = .{
                        .left = try self.desugar(pair.left),
                        .right = try self.desugar(pair.right),
                    }},
                    syntax_expr
                );
            },
            .Intersect => |pair| {
                return try self.store.putCore(
                    .{.Intersect = .{
                        .left = try self.desugar(pair.left),
                        .right = try self.desugar(pair.right),
                    }},
                    syntax_expr
                );
            },
            .Product => |pair| {
                return try self.store.putCore(
                    .{.Product = .{
                        .left = try self.desugar(pair.left),
                        .right = try self.desugar(pair.right),
                    }},
                    syntax_expr
                );
            },
            .Equal => |pair| {
                return try self.store.putCore(
                    .{.Equal = .{
                        .left = try self.desugar(pair.left),
                        .right = try self.desugar(pair.right),
                    }},
                    syntax_expr
                );
            },
            .Name => |name| {
                const args = self.scope.items;
                var i: usize = 0;
                while (i < args.len) : (i += 1) {
                    const arg = args[args.len - 1 - i];
                    if (meta.deepEqual(name, arg.name)) {
                        return self.store.putCore(
                            if (arg.unbox) .{.UnboxName = i} else .{.Name = i},
                            syntax_expr,
                        );
                    }
                }
                return self.desugarError(syntax_expr, "Name not in scope: {}", .{name});
            },
            .When => |when| {
                return try self.store.putCore(
                    .{.When = .{
                        .condition = try self.desugar(when.condition),
                        .true_branch = try self.desugar(when.true_branch),
                    }},
                    syntax_expr
                );
            },
            .Abstract => |abstract| {
                for (abstract.args) |arg| {
                    try self.scope.append(arg);
                }
                var body = try self.desugar(abstract.body);
                for (abstract.args) |_| {
                    _ = self.scope.pop();
                    body = try self.store.putCore(
                        .{.Abstract = body},
                        syntax_expr,
                    );
                }
                return body;
            },
            .Apply => |pair| {
                return try self.store.putCore(
                    .{.Apply = .{
                        .left = try self.desugar(pair.left),
                        .right = try self.desugar(pair.right),
                    }},
                    syntax_expr
                );
            },
            .Box => |expr| {
                const body = try self.desugar(expr);
                const box_id = try self.store.putBox(body);
                var body_name_ixes = DeepHashSet(core.NameIx).init(&self.store.arena.allocator);
                try body.getNameIxes(&body_name_ixes);
                var box_scope = ArrayList(core.NameIx).init(&self.store.arena.allocator);
                var body_name_ixes_iter = body_name_ixes.iterator();
                while (body_name_ixes_iter.next()) |kv| {
                    try box_scope.append(kv.key);
                }
                return try self.store.putCore(
                    .{.Box = .{
                        .id = box_id,
                        .scope = box_scope.items,
                        .body = body,
                    }},
                    syntax_expr,
                );
            },
            .Annotate => |annotate| {
                return try self.store.putCore(
                    .{.Annotate = .{
                        .annotation = annotate.annotation,
                        .body = try self.desugar(annotate.body),
                    }},
                    syntax_expr,
                );
            },
            .Negate => |expr| {
                // `!e` => `e == none`
                TODO();
            },
            .If => |if_| {
                // `if c t f` => `[c] ([g] -> (when g then t) | (when !g then f))`
                TODO();
            },
            .Let => |let| {
                // `let n = v in b` => `[v] ([n] -> b)`
                TODO();
            },
            .Lookup => |lookup| {
                // `a:b` => `(a "b") ([g] -> g)`
                TODO();
            },
        }
    }
};

pub const parse = if (@import("builtin").is_test) @import("./parse.zig");

fn testDesugar(source: []const u8, expected: []const u8) !void {
    var arena = ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var store = Store.init(&arena);
    const syntax_expr = try parse.parse(&store, source, &parse.ParseErrorInfo.init());
    var desugar_error_info = DesugarErrorInfo.init();
    if (desugar(&store, syntax_expr, &desugar_error_info)) |found| {
        var bytes = ArrayList(u8).init(std.testing.allocator);
        defer bytes.deinit();
        try found.dumpInto(bytes.outStream(), 0);
        if (!meta.deepEqual(expected, bytes.items)) {
            panic("\nExpected desugar:\n{}\n\nFound desugar:\n{}", .{expected, bytes.items});
        }
    } else |err| {
        warn("\nExpected desugar:\n{}\n\nFound error:\n{}\n", .{expected, desugar_error_info.message});
        return err;
    }
}

fn testDesugarError(source: []const u8, expected: []const u8) !void {
    var arena = ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var store = Store.init(&arena);
    const syntax_expr = try parse.parse(&store, source, &parse.ParseErrorInfo.init());
    var desugar_error_info = DesugarErrorInfo.init();
    if (desugar(&store, syntax_expr, &desugar_error_info)) |found| {
        var bytes = ArrayList(u8).init(std.testing.allocator);
        try found.dumpInto(bytes.outStream(), 0);
        panic("\nExpected error:\n{}\n\nFound desugar:\n{}", .{expected, bytes.items});
    } else |err| {
        if (!meta.deepEqual(expected, desugar_error_info.message)) {
            warn("\nExpected error:\n{}\n\nFound error:\n{}\n", .{expected, desugar_error_info.message});
            return err;
        }
    }
}

test "desugar" {
    try testDesugar(
        \\\ a [b] c -> a | b . (\ b -> b)
            ,
            \\\ _ ->
            \\  \ _ ->
            \\    \ _ ->
            \\      |
            \\        get 2
            \\        .
            \\          get unbox 1
            \\          \ _ ->
            \\            get 0
    );
}
