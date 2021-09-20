const imp = @import("../../../imp.zig");
usingnamespace imp.common;
const meta = imp.meta;
const Store = imp.lang.Store;
const core = imp.lang.repr.core2;
const syntax = imp.lang.repr.syntax;

pub fn desugar(store: *Store, syntax_expr: *const syntax.Expr, error_info: *?ErrorInfo) Error!core.Program {
    var desugarer = Desugarer{
        .store = store,
        .exprs = ArrayList(*const core.Expr).init(&store.arena.allocator),
        .scope = ArrayList(Desugarer.ScopeItem).init(&store.arena.allocator),
        .current_expr = null,
        .error_info = error_info,
    };
    try desugarer.exprs.append(try desugarer.desugar(syntax_expr));
    return desugarer.exprs.toOwnedSlice();
}

pub const Error = error{
    // sets error_info
    Desugar2Error,

    // does not set error_info
    OutOfMemory,
};

pub const ErrorInfo = struct {
    expr: *const syntax.Expr,
    message: []const u8,
};

// --------------------------------------------------------------------------------

const Desugarer = struct {
    store: *Store,
    exprs: ArrayList(*const core.Expr),
    scope: ArrayList(ScopeItem),
    current_expr: ?*const syntax.Expr,
    error_info: *?ErrorInfo,

    const ScopeItem = struct {
        name: ?syntax.Name,
        kind: union(enum) {
            Scalar,
            UnboxScalar,
            Set: core.Box,
        },
    };

    fn setError(self: *Desugarer, comptime fmt: []const u8, args: anytype) Error {
        const message = try format(&self.store.arena.allocator, fmt, args);
        self.error_info.* = ErrorInfo{
            .expr = self.current_expr.?,
            .message = message,
        };
        return error.Desugar2Error;
    }

    fn putCore(self: *Desugarer, core_expr: core.Expr) !*const core.Expr {
        return self.store.putCore2(core_expr, self.current_expr.?);
    }

    fn scopeAppend(self: *Desugarer, item: ScopeItem) !void {
        if (item.kind != .Set) {
            for (self.scope.items) |other_item| {
                if (other_item.kind == .Set) {
                    for (other_item.kind.Set.args) |*arg|
                        arg.* += 1;
                }
            }
        }
        try self.scope.append(item);
    }

    fn scopePop(self: *Desugarer) !void {
        const item = self.scope.pop();
        if (item.kind != .Set) {
            for (self.scope.items) |other_item| {
                if (other_item.kind == .Set) {
                    for (other_item.kind.Set.args) |*arg|
                        arg.* -= 1;
                }
            }
        }
    }

    fn desugar(self: *Desugarer, syntax_expr: *const syntax.Expr) Error!*const core.Expr {
        const prev_expr = self.current_expr;
        self.current_expr = syntax_expr;
        defer self.current_expr = prev_expr;
        const core_expr = switch (syntax_expr.*) {
            .None => try self.putCore(.None),
            .Some => try self.putCore(.Some),
            .Scalar => |scalar| try self.putCore(.{ .Scalar = scalar }),
            .Union => |pair| try self.putCore(.{
                .Union = .{
                    .left = try self.desugar(pair.left),
                    .right = try self.desugar(pair.right),
                },
            }),
            .Intersect => |pair| try self.putCore(.{
                .Intersect = .{
                    .left = try self.desugar(pair.left),
                    .right = try self.desugar(pair.right),
                },
            }),
            .Product => |pair| try self.putCore(.{
                .Product = .{
                    .left = try self.desugar(pair.left),
                    .right = try self.desugar(pair.right),
                },
            }),
            .Extend => |pair| extend: {
                // `l . r` => `l (?a a , a r)`
                // TODO how should this behave when `l` has arity > 1
                const left = try self.desugar(pair.left);
                try self.scopeAppend(.{
                    .name = null,
                    .kind = .Scalar,
                });
                const right = try self.desugar(pair.right);
                try self.scopePop();
                break :extend try self.putCore(
                    .{ .Apply = .{
                        .left = left,
                        .right = try self.putCore(
                            .{ .Abstract = try self.putCore(
                                .{ .Product = .{
                                    .left = try self.putCore(.{ .ScalarId = 0 }),
                                    .right = try self.putCore(
                                        .{ .Apply = .{
                                            .left = try self.putCore(.{ .ScalarId = 0 }),
                                            .right = right,
                                        } },
                                    ),
                                } },
                            ) },
                        ),
                    } },
                );
            },
            .Equal => |pair| try self.putCore(.{
                .Equal = .{
                    .left = try self.desugar(pair.left),
                    .right = try self.desugar(pair.right),
                },
            }),
            .Name => |name| name: {
                // check for sugar
                if (meta.deepEqual(name, "~")) {
                    break :name try self.putCore(
                        .{ .Abstract = try self.putCore(
                            .{ .Abstract = try self.putCore(
                                .{ .Product = .{
                                    .left = try self.putCore(.{ .ScalarId = 0 }),
                                    .right = try self.putCore(.{ .ScalarId = 1 }),
                                } },
                            ) },
                        ) },
                    );
                }

                // look for name in scope
                const scope = self.scope.items;
                var i: usize = 0;
                var scalar_id: usize = 0;
                while (i < scope.len) : (i += 1) {
                    const item = (scope[scope.len - 1 - i]);
                    if (item.name != null and meta.deepEqual(name, item.name.?)) {
                        break :name switch (item.kind) {
                            .Scalar => try self.putCore(.{ .ScalarId = scalar_id }),
                            .UnboxScalar => try self.putCore(.{ .UnboxScalarId = scalar_id }),
                            .Set => |box| try self.boxToExpr(box),
                        };
                    }
                    if (item.kind != .Set) scalar_id += 1;
                }

                // otherwise look for native
                const native = core.Native.fromName(name) orelse
                    return self.setError("Name not in scope: {s}", .{name});
                break :name try self.putCore(.{ .Native = native });
            },
            .Negate => |expr| try self.putCore(.{ .Negate = try self.desugar(expr) }),
            .Then => |then| try self.putCore(.{
                .Then = .{
                    .condition = try self.desugar(then.condition),
                    .true_branch = try self.desugar(then.true_branch),
                },
            }),
            .Abstract => |abstract| abstract: {
                try self.scopeAppend(.{
                    .name = abstract.arg.name,
                    .kind = if (abstract.arg.unbox) .UnboxScalar else .Scalar,
                });
                const body = try self.desugar(abstract.body);
                try self.scopePop();
                break :abstract try self.putCore(.{ .Abstract = body });
            },
            .Apply => |pair| try self.putCore(.{
                .Apply = .{
                    .left = try self.desugar(pair.left),
                    .right = try self.desugar(pair.right),
                },
            }),
            .Box => |expr| try self.putCore(.{
                .Box = try self.makeDef(try self.desugar(expr)),
            }),
            .Fix => |fix| try self.putCore(.{
                .Fix = .{
                    .init = try self.desugar(fix.init),
                    .next = try self.desugar(fix.next),
                },
            }),
            .Reduce => |reduce| try self.putCore(.{
                .Reduce = .{
                    .input = try self.desugar(reduce.input),
                    .init = try self.desugar(reduce.init),
                    .next = try self.desugar(reduce.next),
                },
            }),
            .Enumerate => |body| try self.putCore(.{ .Enumerate = try self.desugar(body) }),
            .Annotate => |annotate| try self.putCore(.{
                .Annotate = .{
                    .annotation = annotate.annotation,
                    .body = try self.desugar(annotate.body),
                },
            }),
            .ThenElse => |then_else| then_else: {
                // `c then t else f` => `tmp: c; ((tmp then t) | (tmp! then f))`
                const tmp = try self.boxToExpr(try self.makeDef(try self.desugar(then_else.condition)));
                break :then_else try self.putCore(.{
                    .Union = .{
                        .left = try self.putCore(.{
                            .Then = .{
                                .condition = tmp,
                                .true_branch = try self.desugar(then_else.true_branch),
                            },
                        }),
                        .right = try self.putCore(.{
                            .Then = .{
                                .condition = try self.putCore(.{ .Negate = tmp }),
                                .true_branch = try self.desugar(then_else.false_branch),
                            },
                        }),
                    },
                });
            },
            .Def => |def| def: {
                const value = value: {
                    if (def.fix) {
                        // `fix n: v;` => `n: fix none ?@n v;`
                        try self.scopeAppend(.{
                            .name = def.name,
                            .kind = .UnboxScalar,
                        });
                        const fix_body = try self.desugar(def.value);
                        try self.scopePop();
                        break :value try self.putCore(.{
                            .Fix = .{
                                .init = try self.putCore(.None),
                                .next = try self.putCore(.{ .Abstract = fix_body }),
                            },
                        });
                    } else {
                        break :value try self.desugar(def.value);
                    }
                };
                try self.scopeAppend(.{
                    .name = def.name,
                    .kind = .{ .Set = try self.makeDef(value) },
                });
                const body = try self.desugar(def.body);
                try self.scopePop();
                break :def body;
            },
        };
        return core_expr;
    }

    fn makeDef(self: *Desugarer, expr: *const core.Expr) Error!core.Box {
        var free_ids = DeepHashSet(core.ScalarId).init(&self.store.arena.allocator);
        const StackItem = struct {
            num_enclosing_abstracts: usize,
            expr: *const core.Expr,
        };
        {
            var stack = ArrayList(StackItem).init(&self.store.arena.allocator);
            try stack.append(.{ .num_enclosing_abstracts = 0, .expr = expr });
            while (stack.popOrNull()) |item| {
                switch (item.expr.*) {
                    .ScalarId, .UnboxScalarId => |scalar_id| {
                        if (scalar_id >= item.num_enclosing_abstracts) {
                            try free_ids.put(scalar_id - item.num_enclosing_abstracts, {});
                        }
                    },
                    else => {},
                }
                for (item.expr.getChildren().slice()) |child|
                    try stack.append(.{
                        .num_enclosing_abstracts = item.num_enclosing_abstracts + (if (item.expr.* == .Abstract) @as(usize, 1) else 0),
                        .expr = child,
                    });
            }
        }

        var args = ArrayList(core.ScalarId).init(&self.store.arena.allocator);
        {
            var iter = free_ids.iterator();
            while (iter.next()) |entry| try args.append(entry.key_ptr.*);
        }
        var result_expr = try self.rename(args.items, expr, 0);
        for (args.items) |_| {
            result_expr = try self.putCore(.{ .Abstract = result_expr });
        }
        try self.exprs.append(result_expr);
        const set_id = self.exprs.items.len - 1;
        return core.Box{
            .set_id = set_id,
            .args = args.toOwnedSlice(),
        };
    }

    fn rename(self: *Desugarer, args: []const core.ScalarId, expr: *const core.Expr, num_enclosing_abstracts: usize) Error!*const core.Expr {
        var tmp_expr = expr.*;
        switch (tmp_expr) {
            .ScalarId, .UnboxScalarId => |*scalar_id| {
                if (scalar_id.* >= num_enclosing_abstracts) {
                    for (args) |arg, i| {
                        if (arg == scalar_id.* - num_enclosing_abstracts) {
                            scalar_id.* = args.len - 1 - i + num_enclosing_abstracts;
                        }
                    }
                }
            },
            else => {},
        }
        for (tmp_expr.getChildrenMut().slice()) |child|
            child.* = try self.rename(args, child.*, num_enclosing_abstracts + (if (tmp_expr == .Abstract) @as(usize, 1) else 0));
        const expr_meta = Store.getCore2Meta(expr);
        return self.store.putCore2(tmp_expr, expr_meta.from);
    }

    fn makeBox(self: *Desugarer, body: *const core.Expr) Error!*const core.Expr {
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
        return self.putCore(.{
            .Box = .{
                .body = body,
                .scope = scope,
            },
        });
    }

    fn boxToExpr(self: *Desugarer, box: core.Box) !*const core.Expr {
        var result = try self.putCore(.{ .SetId = box.set_id });
        for (box.args) |arg| {
            result = try self.putCore(.{
                .Apply = .{
                    .left = result,
                    .right = try self.putCore(.{ .ScalarId = arg }),
                },
            });
        }
        return result;
    }
};
