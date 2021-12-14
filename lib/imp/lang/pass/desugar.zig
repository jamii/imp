const imp = @import("../../../imp.zig");
const u = imp.util;
const core = imp.lang.repr.core;
const syntax = imp.lang.repr.syntax;

pub fn desugar(
    arena: *u.ArenaAllocator,
    syntax_program: syntax.Program,
    watch_expr_id: ?syntax.ExprId,
    error_info: *?ErrorInfo,
) Error!core.Program {
    var desugarer = Desugarer{
        .arena = arena,
        .syntax_program = syntax_program,
        .watch_expr_id = watch_expr_id,
        .defs = u.ArrayList(core.ExprId).init(&arena.allocator),
        .exprs = u.ArrayList(core.Expr).init(&arena.allocator),
        .from_syntax = u.ArrayList(syntax.ExprId).init(&arena.allocator),
        .scope = u.ArrayList(Desugarer.ScopeItem).init(&arena.allocator),
        .current_expr_id = null,
        .error_info = error_info,
    };
    try desugarer.defs.append(try desugarer.desugar(.{ .id = syntax_program.exprs.len - 1 }));
    return core.Program{
        .defs = desugarer.defs.toOwnedSlice(),
        .exprs = desugarer.exprs.toOwnedSlice(),
        .from_syntax = desugarer.from_syntax.toOwnedSlice(),
    };
}

pub const Error = error{
    // sets error_info
    DesugarError,

    // does not set error_info
    OutOfMemory,
};

pub const ErrorInfo = struct {
    expr_id: syntax.ExprId,
    message: []const u8,
};

// --------------------------------------------------------------------------------

const Desugarer = struct {
    arena: *u.ArenaAllocator,
    syntax_program: syntax.Program,
    watch_expr_id: ?syntax.ExprId,
    defs: u.ArrayList(core.ExprId),
    exprs: u.ArrayList(core.Expr),
    from_syntax: u.ArrayList(syntax.ExprId),
    scope: u.ArrayList(ScopeItem),
    current_expr_id: ?syntax.ExprId,
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
        const message = try u.formatToString(&self.arena.allocator, fmt, args);
        self.error_info.* = ErrorInfo{
            .expr_id = self.current_expr_id.?,
            .message = message,
        };
        return error.DesugarError;
    }

    fn putCore(self: *Desugarer, core_expr: core.Expr) !core.ExprId {
        try self.exprs.append(core_expr);
        try self.from_syntax.append(self.current_expr_id.?);
        return core.ExprId{ .id = self.exprs.items.len - 1 };
    }

    fn scopeAppend(self: *Desugarer, item: ScopeItem) !void {
        if (item.kind != .Set) {
            for (self.scope.items) |other_item| {
                if (other_item.kind == .Set) {
                    for (other_item.kind.Set.args) |*arg|
                        arg.id += 1;
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
                        arg.id -= 1;
                }
            }
        }
    }

    fn desugar(self: *Desugarer, syntax_expr_id: syntax.ExprId) Error!core.ExprId {
        const prev_expr_id = self.current_expr_id;
        self.current_expr_id = syntax_expr_id;
        defer self.current_expr_id = prev_expr_id;
        const syntax_expr = self.syntax_program.exprs[syntax_expr_id.id];
        const core_expr_id = switch (syntax_expr) {
            .None => try self.putCore(.None),
            .Some => try self.putCore(.Some),
            .Scalar => |scalar| try self.putCore(.{
                .Scalar = switch (scalar) {
                    .Text => |text| .{ .Text = text },
                    .Number => |number| .{ .Number = number },
                    .Box => u.imp_panic("Shouldn't be any box literals", .{}),
                },
            }),
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
                                    .left = try self.putCore(.{ .ScalarId = .{ .id = 0 } }),
                                    .right = try self.putCore(
                                        .{ .Apply = .{
                                            .left = try self.putCore(.{ .ScalarId = .{ .id = 0 } }),
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
                if (u.deepEqual(name, "~")) {
                    break :name try self.putCore(
                        .{ .Abstract = try self.putCore(
                            .{ .Abstract = try self.putCore(
                                .{ .Product = .{
                                    .left = try self.putCore(.{ .ScalarId = .{ .id = 0 } }),
                                    .right = try self.putCore(.{ .ScalarId = .{ .id = 1 } }),
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
                    if (item.name != null and u.deepEqual(name, item.name.?)) {
                        break :name switch (item.kind) {
                            .Scalar => try self.putCore(.{ .ScalarId = .{ .id = scalar_id } }),
                            .UnboxScalar => try self.putCore(.{ .UnboxScalarId = .{ .id = scalar_id } }),
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
                .Box = try self.putDef(try self.desugar(expr)),
            }),
            .Fix => |fix| try self.boxToExpr(try self.putDef(try self.putCore(.{
                .Fix = .{
                    .init = try self.desugar(fix.init),
                    .next = try self.putDef(try self.desugar(fix.next)),
                },
            }))),
            .Reduce => |reduce| try self.boxToExpr(try self.putDef(try self.putCore(.{
                .Reduce = .{
                    .input = try self.desugar(reduce.input),
                    .init = try self.desugar(reduce.init),
                    .next = try self.putDef(try self.desugar(reduce.next)),
                },
            }))),
            .Enumerate => |body| try self.putCore(.{ .Enumerate = try self.desugar(body) }),
            .Annotate => |annotate| try self.putCore(.{
                .Annotate = .{
                    .annotation = annotate.annotation,
                    .body = try self.desugar(annotate.body),
                },
            }),
            .ThenElse => |then_else| then_else: {
                // `c then t else f` => `tmp: c; ((tmp then t) | (tmp! then f))`
                const tmp = try self.boxToExpr(try self.putDef(try self.desugar(then_else.condition)));
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
                                .next = try self.putDef(try self.putCore(.{ .Abstract = fix_body })),
                            },
                        });
                    } else {
                        break :value try self.desugar(def.value);
                    }
                };
                try self.scopeAppend(.{
                    .name = def.name,
                    .kind = .{ .Set = try self.putDef(value) },
                });
                const body = try self.desugar(def.body);
                try self.scopePop();
                break :def body;
            },
        };
        if (self.watch_expr_id) |watch_expr_id| {
            if (watch_expr_id.id == syntax_expr_id.id) {
                var watch_scope = u.ArrayList(core.Watch.ScopeItem).init(&self.arena.allocator);
                var i: usize = 0;
                var scalar_id: usize = 0;
                while (i < self.scope.items.len) : (i += 1) {
                    const item = self.scope.items[self.scope.items.len - 1 - i];
                    if (item.kind != .Set) {
                        if (item.name) |name|
                            try watch_scope.append(.{ .name = name, .scalar_id = .{ .id = scalar_id } });
                        scalar_id += 1;
                    }
                }
                return self.putCore(.{ .Watch = .{ .scope = watch_scope.toOwnedSlice(), .body = core_expr_id } });
            }
        }
        return core_expr_id;
    }

    fn putDef(self: *Desugarer, expr_id: core.ExprId) Error!core.Box {
        // get list of free scalar_ids in expr
        var free_ids = u.DeepHashSet(core.ScalarId).init(&self.arena.allocator);
        for (try self.getDescendants(expr_id)) |descendant|
            for (try self.getScalarIds(descendant.expr_id)) |scalar_id|
                if (scalar_id.id >= descendant.num_enclosing_abstracts)
                    try free_ids.put(.{ .id = scalar_id.id - descendant.num_enclosing_abstracts }, {});

        // wrap expr with one abstract for each free scalar_id
        var args = u.ArrayList(core.ScalarId).init(&self.arena.allocator);
        {
            var iter = free_ids.iterator();
            while (iter.next()) |entry| try args.append(entry.key_ptr.*);
        }
        var result_expr_id = expr_id;
        for (args.items) |_| {
            result_expr_id = try self.putCore(.{ .Abstract = result_expr_id });
        }

        // rename free scalar_ids in expr to match position in args
        for (try self.getDescendants(expr_id)) |descendant| {
            for (try self.getScalarIds(descendant.expr_id)) |scalar_id| {
                if (scalar_id.id >= descendant.num_enclosing_abstracts) {
                    for (args.items) |arg, i| {
                        if (scalar_id.id - descendant.num_enclosing_abstracts == arg.id) {
                            scalar_id.id = args.items.len - 1 - i + descendant.num_enclosing_abstracts;
                            break;
                        }
                    }
                }
            }
        }

        // make def
        try self.defs.append(result_expr_id);
        const def_id = core.DefId{ .id = self.defs.items.len - 1 };
        return core.Box{
            .def_id = def_id,
            .args = args.toOwnedSlice(),
        };
    }

    const Descendant = struct {
        num_enclosing_abstracts: usize,
        expr_id: core.ExprId,
    };
    fn getDescendants(self: *Desugarer, expr_id: core.ExprId) ![]const Descendant {
        var stack = u.ArrayList(Descendant).init(&self.arena.allocator);
        var result = u.ArrayList(Descendant).init(&self.arena.allocator);
        try stack.append(.{ .num_enclosing_abstracts = 0, .expr_id = expr_id });
        while (stack.popOrNull()) |descendant| {
            try result.append(descendant);
            const expr = self.exprs.items[descendant.expr_id.id];
            for (expr.getChildren().slice()) |child|
                try stack.append(.{
                    .num_enclosing_abstracts = descendant.num_enclosing_abstracts + (if (expr == .Abstract) @as(usize, 1) else 0),
                    .expr_id = child,
                });
        }
        return result.toOwnedSlice();
    }

    fn getScalarIds(self: *Desugarer, expr_id: core.ExprId) ![]const *core.ScalarId {
        var scalar_ids = u.ArrayList(*core.ScalarId).init(&self.arena.allocator);
        switch (self.exprs.items[expr_id.id]) {
            .ScalarId, .UnboxScalarId => |*scalar_id| try scalar_ids.append(scalar_id),
            .Box => |*box| for (box.args) |*scalar_id| try scalar_ids.append(scalar_id),
            .Reduce => |*reduce| for (reduce.next.args) |*scalar_id| try scalar_ids.append(scalar_id),
            .Fix => |*fix| for (fix.next.args) |*scalar_id| try scalar_ids.append(scalar_id),
            .Watch => |*watch| for (watch.scope) |*scope_item| try scalar_ids.append(&scope_item.scalar_id),
            else => {},
        }
        return scalar_ids.toOwnedSlice();
    }

    fn makeBox(self: *Desugarer, body: core.ExprId) Error!core.ExprId {
        var scope = try self.arena.allocator.alloc(core.NameIx, self.scope.items.len);
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

    fn boxToExpr(self: *Desugarer, box: core.Box) !core.ExprId {
        var result = try self.putCore(.{ .DefId = box.def_id });
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
