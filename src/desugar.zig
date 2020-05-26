usingnamespace @import("./common.zig");

const Store = @import("./store.zig").Store;
const syntax = @import("./syntax.zig");
const core = @import("./core.zig");

pub fn desugar(store: *Store, syntax_expr: *const syntax.Expr, error_info: *?ErrorInfo) Error ! *const core.Expr {
    var desugarer = Desugarer{
        .store = store,
        .scope = ArrayList(?syntax.Arg).init(&store.arena.allocator),
        .current_expr = null,
        .error_info = error_info,
    };
    return desugarer.desugar(syntax_expr);
}

pub const Error = error {
    // sets error_info
    DesugarError,

    // does not set error_info
    OutOfMemory,
};

pub const ErrorInfo = struct {
    expr: ?*const syntax.Expr,
    message: []const u8,
};

// --------------------------------------------------------------------------------

const Desugarer = struct {
    store: *Store,
    scope: ArrayList(?syntax.Arg), // null when unnameable
    current_expr: ?*const syntax.Expr,
    error_info: *?ErrorInfo,

    fn setError(self: *Desugarer, comptime fmt: []const u8, args: var) Error {
        const message = try format(&self.store.arena.allocator, fmt, args);
        self.error_info.* = ErrorInfo{
            .expr = self.current_expr,
            .message = message,
        };
        return error.DesugarError;
    }

    fn putCore(self: *Desugarer, core_expr: core.Expr) Error ! *const core.Expr {
        return self.store.putCore(core_expr, self.current_expr.?);
    }

    fn desugar(self: *Desugarer, syntax_expr: *const syntax.Expr) Error ! *const core.Expr {
        const prev_expr = self.current_expr;
        self.current_expr = syntax_expr;
        const core_expr = switch (syntax_expr.*) {
            .None => try self.putCore(.None),
            .Some => try self.putCore(.Some),
            .Scalar => |scalar| try self.putCore(.{.Scalar = scalar}),
            .Union => |pair|
                try self.putCore(
                    .{.Union = .{
                        .left = try self.desugar(pair.left),
                        .right = try self.desugar(pair.right),
                    }}
            ),
            .Intersect => |pair|
                try self.putCore(
                    .{.Intersect = .{
                        .left = try self.desugar(pair.left),
                        .right = try self.desugar(pair.right),
                    }}
            ),
            .Product => |pair|
                try self.putCore(
                    .{.Product = .{
                        .left = try self.desugar(pair.left),
                        .right = try self.desugar(pair.right),
                    }}
            ),
            .Equal => |pair|
                try self.putCore(
                    .{.Equal = .{
                        .left = try self.desugar(pair.left),
                        .right = try self.desugar(pair.right),
                    }}
                ),
            .Name => |name| name: {
                const args = self.scope.items;
                var i: usize = 0;
                while (i < args.len) : (i += 1) {
                    if (args[args.len - 1 - i]) |arg| {
                        if (meta.deepEqual(name, arg.name)) {
                            break :name try self.putCore(
                                if (arg.unbox) .{.UnboxName = i} else .{.Name = i},
                            );
                        }
                    }
                }
                return self.setError("Name not in scope: {}", .{name});
            },
            .When => |when|
                try self.putCore(
                    .{.When = .{
                        .condition = try self.desugar(when.condition),
                        .true_branch = try self.desugar(when.true_branch),
                    }}
            ),
            .Abstract => |abstract| abstract: {
                for (abstract.args) |arg| {
                    try self.scope.append(arg);
                }
                var body = try self.desugar(abstract.body);
                for (abstract.args) |_| {
                    _ = self.scope.pop();
                    body = try self.putCore(
                        .{.Abstract = body},
                    );
                }
                break :abstract body;
            },
            .Apply => |pair|
                return self.putCore(
                    .{.Apply = .{
                        .left = try self.desugar(pair.left),
                        .right = try self.desugar(pair.right),
                    }},
            ),
            .Box => |expr| try self.desugarBox(expr),
            .Annotate => |annotate|
                try self.putCore(
                    .{.Annotate = .{
                        .annotation = annotate.annotation,
                        .body = try self.desugar(annotate.body),
                    }},
            ),
            .Negate => |expr| negate: {
                // `!e` => `e == none`
                const e = try self.desugar(expr);
                const none = try self.putCore(.None);
                break :negate try self.putCore(.{.Equal = .{.left=e, .right=none}});
            },
            .If => |if_| if_: {
                // `if c t f` => `[c] ([g] -> (when g then t) | (when !g then f))`
                const box_c = try self.desugarBox(if_.condition);
                try self.scope.append(null); // g
                const t = try self.desugar(if_.true_branch);
                const f = try self.desugar(if_.false_branch);
                _ = self.scope.pop();
                const left_g = try self.putCore(.{.UnboxName = 0});
                const left = try self.putCore(.{.When = .{.condition=left_g, .true_branch=t}});
                const right_g = try self.putCore(.{.UnboxName = 0});
                const none = try self.putCore(.None);
                const not_right_g = try self.putCore(.{.Equal = .{.left=right_g, .right=none}});
                const right = try self.putCore(.{.When = .{.condition=not_right_g, .true_branch=f}});
                const body = try self.putCore(.{.Union = .{.left=left, .right=right}});
                const abstract = try self.putCore(.{.Abstract = body});
                break :if_ try self.putCore(.{.Apply = .{.left=box_c, .right=abstract}});
            },
            .Let => |let| let: {
                // `let n = v in b` => `[v] ([n] -> b)`
                const box_v = try self.desugarBox(let.value);
                try self.scope.append(syntax.Arg{.name=let.name, .unbox=true});
                const b = try self.desugar(let.body);
                _ = self.scope.pop();
                const abstract = try self.putCore(.{.Abstract = b});
                break :let try self.putCore(.{.Apply=.{.left=box_v, .right=abstract}});
            },
            .Lookup => |lookup| lookup: {
                // `a:b` => `(a "b") ([g] -> g)`
                const a = try self.desugar(lookup.value);
                const b = try self.putCore(.{.Scalar = .{.String = lookup.name}});
                const apply_ab = try self.putCore(.{.Apply = .{.left=a, .right=b}});
                const unbox = try self.putCore(.{.UnboxName = 0});
                const abstract = try self.putCore(.{.Abstract = unbox});
                break :lookup try self.putCore(.{.Apply = .{.left=apply_ab, .right=abstract}});
            },
        };
        self.current_expr = prev_expr;
        return core_expr;
    }

    fn desugarBox(self: *Desugarer, syntax_expr: *const syntax.Expr) Error ! *const core.Expr {
        const body = try self.desugar(syntax_expr);
        var body_name_ixes = DeepHashSet(core.NameIx).init(&self.store.arena.allocator);
        try body.getNameIxes(&body_name_ixes);
        var box_scope = ArrayList(core.NameIx).init(&self.store.arena.allocator);
        var body_name_ixes_iter = body_name_ixes.iterator();
        while (body_name_ixes_iter.next()) |kv| {
            try box_scope.append(kv.key);
        }
        return self.putCore(
            .{.Box = .{
                .scope = box_scope.items,
                .body = body,
            }}
        );
    }
};

const parse = if (builtin.is_test) @import("./parse.zig");

fn testDesugar(source: []const u8, expected: []const u8) !void {
    var arena = ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var store = Store.init(&arena);
    var parse_error_info: ?parse.ErrorInfo = null;
    const syntax_expr = try parse.parse(&store, source, &parse_error_info);
    var error_info: ?ErrorInfo = null;
    if (desugar(&store, syntax_expr, &error_info)) |found| {
        var bytes = ArrayList(u8).init(std.testing.allocator);
        defer bytes.deinit();
        try found.dumpInto(bytes.outStream(), 0);
        if (!meta.deepEqual(expected, bytes.items)) {
            panic("\nExpected desugar:\n{}\n\nFound desugar:\n{}", .{expected, bytes.items});
        }
    } else |err| {
        warn("\nExpected desugar:\n{}\n\nFound error:\n{}\n", .{expected, error_info.?.message});
        return err;
    }
}

fn testError(source: []const u8, expected: []const u8) !void {
    var arena = ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var store = Store.init(&arena);
    var parse_error_info: ?parse.ErrorInfo = null;
    const syntax_expr = try parse.parse(&store, source, &parse_error_info);
    var error_info: ?ErrorInfo = null;
    if (desugar(&store, syntax_expr, &error_info)) |found| {
        var bytes = ArrayList(u8).init(std.testing.allocator);
        try found.dumpInto(bytes.outStream(), 0);
        panic("\nExpected error:\n{}\n\nFound desugar:\n{}", .{expected, bytes.items});
    } else |err| {
        if (!meta.deepEqual(expected, error_info.?.message)) {
            warn("\nExpected error:\n{}\n\nFound error:\n{}\n", .{expected, error_info.?.message});
            return err;
        }
    }
}

test "desugar" {
    try testError(
        \\\ a [b] -> a | b . c
            ,
            \\Name not in scope: c
    );

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

    try testDesugar(
        \\!1
            ,
            \\=
            \\  1
            \\  none
    );

    try testDesugar(
        \\if (1 = 2) then 3 else 4
            ,
            \\apply
            \\  box;
            \\    =
            \\      1
            \\      2
            \\  \ _ ->
            \\    |
            \\      when
            \\        get unbox 0
            \\        3
            \\      when
            \\        =
            \\          get unbox 0
            \\          none
            \\        4
    );

    try testDesugar(
        \\let a = 1 in a
            ,
            \\apply
            \\  box;
            \\    1
            \\  \ _ ->
            \\    get unbox 0
    );

    try testDesugar(
        \\let a = 1 in let b = a in a
            ,
            \\apply
            \\  box;
            \\    1
            \\  \ _ ->
            \\    apply
            \\      box; 0
            \\        get unbox 0
            \\      \ _ ->
            \\        get unbox 1
    );

    try testDesugar(
        \\1:foo
            ,
            \\apply
            \\  apply
            \\    1
            \\    "foo"
            \\  \ _ ->
            \\    get unbox 0
    );
}
