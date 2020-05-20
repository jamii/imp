usingnamespace @import("./common.zig");

const Store = @import("./store.zig").Store;
const syntax = @import("./syntax.zig");
const core = @import("./core.zig");

pub fn desugar(store: *Store, syntax_expr: *const syntax.Expr, desugar_error_info: *DesugarErrorInfo) DesugarError ! *const core.Expr {
    var desugarer = Desugarer{
        .store = store,
        .scope = ArrayList(name).init(*store.arena.allocator),
        .desugar_error_info = desugar_error_info,
    };
    return desugar.desugar(syntax_expr);
}

pub const DesugarError = error {
    DesugarError,
    OutOfMemory,
};

pub const DesugarErrorInfo = struct {
    expr: ?*const syntax.Expr,
    message: []const u8,

    fn init() DesugarErrorInfo {
        return DesugarErrorInfo{
            .expr = null,
            .message = "not an error",
        };
    }
};

// --------------------------------------------------------------------------------

const Desugarer = struct {
    store: *Store,
    scope: ArrayList(name),
    desugar_error_info: DesugarErrorInfo,

    fn desugar_error(self: *Desugar, syntax_expr: *const syntax.Expr, fmt: []const u8, args: var) DesugarError {
        self.desugar_error_info.expr = syntax_expr;
        self.desugar_error_info.message = format(fmt, args);
        return error.DesugarError;
    }

    // fn desugar_let

    fn desugar(self: *Desugarer, syntax_expr: *const syntax.Expr) Desugar ! *const core.Expr {
        switch (syntax_expr) {
            .None => return try self.store.put_core(.None, syntax_expr),
            .Some => return try self.store.put_core(.Some, syntax_expr),
            .Scalar => |scalar| return try self.store.put_core(.{.Scalar = scalar}, syntax_expr),
            .Union => |pair| {
                return try self.store.put_core(
                    .{.Union = .{
                        .left = try self.desugar(pair.left),
                        .right = try self.desugar(pair.right),
                    }},
                    syntax_expr
                );
            },
            .Intersect => |pair| {
                return try self.store.put_core(
                    .{.Intersect = .{
                        .left = try self.desugar(pair.left),
                        .right = try self.desugar(pair.right),
                    }},
                    syntax_expr
                );
            },
            .Product => |pair| {
                return try self.store.put_core(
                    .{.Product = .{
                        .left = try self.desugar(pair.left),
                        .right = try self.desugar(pair.right),
                    }},
                    syntax_expr
                );
            },
            .Equal => |pair| {
                return try self.store.put_core(
                    .{.Equal = .{
                        .left = try self.desugar(pair.left),
                        .right = try self.desugar(pair.right),
                    }},
                    syntax_expr
                );
            },
            .Name => |name| {
                const names = self.scope.items;
                var i = 0;
                while (i < names.len) : (i += 1) {
                    if (deepEqual(name, names[names.len - 1 - i])) {
                        return self.store.put_core(
                            .{.Name = i},
                            syntax_expr,
                        );
                    }
                }
                return self.desugar_error(syntax_expr, "Name not in scope: {}", name);
            },
            .When => |when| {
                return try self.store.put_core(
                    .{.When = .{
                        .condition = try self.desugar(when.condition),
                        .true_branch = try self.desugar(when.true_branch),
                    }},
                    syntax_expr
                );
            },
            .Abstract => |abstract| {
                for (abstract.args) |arg| {
                    try self.scope.append(arg.name);
                }
                const body = try self.desugar(abstract.body);
                for (abstract.args) |arg| {
                    _ = self.scope.pop();
                }
                var i = abstract.args.len - 1;
                while (i >= 0) : (i -= 1) {
                    expr = try self.store.put_core(
                        .{.Abstract = .{
                            .unbox = abstract.args[i].unbox,
                            .body = expr,
                        }},
                        syntax_expr,
                    );
                }
                self.scope = prev_scope;
                break :root_expr body
            },
            .Apply => |pair| {
                return try self.store.put_core(
                    .{.Apply = .{
                        .left = try self.desugar(pair.left),
                        .right = try self.desugar(pair.right),
                    }},
                    syntax_expr
                );
            },
            .Box => |expr| {
                const body = try self.desugar(expr);
                const box_id = try store.box_expr(body);
                // TODO only use names that are closed over in body
                var box_scope = self.store.arena.allocator.alloc(NameIx, self.scope.len);
                var name_ix = 0;
                while (name_ix < self.scope.items.len) {
                    box_scope[name_ix] = name_ix;
                }
                return try self.store.put_core(
                    .{.Box = .{
                        .id = box_id,
                        .scope = box_scope.items,
                        .body = body,
                    }},
                    syntax_expr,
                );
            },
            .Annotate => |annotate| {
                return try self.store.put_core(
                    .Annotate = {
                        .annotation = annotate.annotation,
                        .body = try self.desugar(annotate.body),
                    },
                    syntax_expr,
                );
            },
            .Negate => |expr| {
                // `!e` => `e == none`
            },
            .If => |if_| {
                // `if c t f` => `[c] ([g] -> (when g then t) | (when !g then f))`
            },
            .Let => |let| {
                // `let n = v in b` => `[v] ([n] -> b)`
            },
            .Lookup => |lookup| {
                // `a:b` => `(a "b") ([g] -> g)`
            },
        }
    }
}
