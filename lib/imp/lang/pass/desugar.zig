const imp = @import("../../../imp.zig");
usingnamespace imp.common;
const meta = imp.meta;
const Store = imp.lang.Store;
const core = imp.lang.repr.core;
const syntax = imp.lang.repr.syntax;

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
                // look for name in scope
                while (i < args.len) : (i += 1) {
                    if (args[args.len - 1 - i]) |arg| {
                        if (meta.deepEqual(name, arg.name)) {
                            break :name try self.putCore(
                                if (arg.unbox) .{.UnboxName = i} else .{.Name = i},
                            );
                        }
                    }
                }
                // otherwise look for native
                break :name try self.putCore(.{.Native = native: {
                    if (meta.deepEqual(name, "+")) {
                        break :native .Add;
                    } else if (meta.deepEqual(name, "-")) {
                        break :native .Subtract;
                    }  else if (meta.deepEqual(name, "*")) {
                        break :native .Multiply;
                    }  else if (meta.deepEqual(name, "/")) {
                        break :native .Divide;
                    } else {
                        return self.setError("Name not in scope: {}", .{name});
                    }
                }});

            },
            .Negate => |expr|
                try self.putCore(
                    .{.Negate = try self.desugar(expr)}
            ),
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
                const not_right_g = try self.putCore(.{.Negate = right_g});
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
                const b = try self.putCore(.{.Scalar = .{.Text = lookup.name}});
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
        // TODO this is correct, but needs to be done everywhere
        // var body_name_ixes = DeepHashSet(core.NameIx).init(&self.store.arena.allocator);
        // try body.getNameIxes(&body_name_ixes);
        // var box_scope = ArrayList(core.NameIx).init(&self.store.arena.allocator);
        // var body_name_ixes_iter = body_name_ixes.iterator();
        // while (body_name_ixes_iter.next()) |kv| {
        //     try box_scope.append(kv.key);
        // }
        var scope = try self.store.arena.allocator.alloc(core.NameIx, self.scope.items.len);
        for (self.scope.items) |_, i| {
            scope[i] = i;
        }
        return self.putCore(
            .{.Box = .{
                .body = body,
                .scope = scope,
            }}
        );
    }
};
