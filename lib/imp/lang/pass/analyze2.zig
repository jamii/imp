const imp = @import("../../../imp.zig");
usingnamespace imp.common;
const meta = imp.meta;
const Store = imp.lang.Store;
const core = imp.lang.repr.core2;
const type_ = imp.lang.repr.type_2;

pub fn analyze(store: *Store, program: core.Program, interrupter: imp.lang.Interrupter, error_info: *?ErrorInfo) Error!type_.ProgramType {
    var expr_type_unions = try store.arena.allocator.alloc(ArrayList(type_.Specialization), program.exprs.len);
    for (expr_type_unions) |*expr_type_union|
        expr_type_union.* = ArrayList(type_.Specialization).init(&store.arena.allocator);
    var analyzer = Analyzer{
        .store = store,
        .program = program,
        .expr_type_unions = expr_type_unions,
        .scope = ArrayList(type_.ScalarType).init(&store.arena.allocator),
        .interrupter = interrupter,
        .error_info = error_info,
    };
    const program_type = try analyzer.analyzeDef(program.exprs.len - 1, &[0]type_.ScalarType{});
    return type_.ProgramType{
        .expr_type_unions = analyzer.expr_type_unions,
        .program_type = program_type,
    };
}

pub const Error = error{
    // sets error_info
    Analyze2Error,

    // does not set error_info
    OutOfMemory,
    WasInterrupted,
};

pub const ErrorInfo = struct {
    expr: *const core.Expr,
    // TODO want something like a stack of specializations
    //callstack: []const struct {
    //    set_id: SetId,
    //    hint: []const type_.ScalarType,
    //},
    message: []const u8,
};

// --------------------------------------------------------------------------------

pub const Analyzer = struct {
    store: *Store,
    program: core.Program,
    expr_type_unions: []ArrayList(type_.Specialization),
    // TODO maybe scope should be passed as an arg like hint
    scope: ArrayList(type_.ScalarType),
    interrupter: imp.lang.Interrupter,
    error_info: *?ErrorInfo,

    fn setError(self: *Analyzer, expr: *const core.Expr, comptime fmt: []const u8, args: anytype) Error {
        const message = try format(&self.store.arena.allocator, fmt, args);
        self.error_info.* = ErrorInfo{
            .expr = expr,
            .message = message,
        };
        return error.Analyze2Error;
    }

    fn analyzeDef(self: *Analyzer, set_id: core.SetId, hint: []const type_.ScalarType) Error!type_.SetType {
        const type_union = &self.expr_type_unions[set_id];

        // see if we already analyzed this specialization
        for (type_union.items) |specialization| {
            // if could specialize with a shorter hint, can specialize with this one
            if (specialization.hint.len <= hint.len and
                meta.deepEqual(specialization.hint, hint[0..specialization.hint.len]))
                return specialization.set_type;
        }

        // otherwise, analyze
        const old_scope = self.scope;
        defer self.scope = old_scope;
        self.scope = ArrayList(type_.ScalarType).init(&self.store.arena.allocator);
        const set_type = try self.analyzeExpr(self.program.exprs[set_id], hint);

        // memoize result
        const used_hint = switch (set_type) {
            // always empty so can't say how much of the hint was used
            .None => hint,
            // specialized, so cannot have used >concrete.columns.len of the hint
            .Concrete => |concrete| hint[0..min(hint.len, concrete.columns.len)],
        };
        try self.expr_type_unions[set_id].append(.{
            .hint = used_hint,
            .set_type = set_type,
        });

        return set_type;
    }

    fn analyzeBox(self: *Analyzer, box: core.Box, hint: []const type_.ScalarType) Error!type_.SetType {
        var box_hint = ArrayList(type_.ScalarType).init(&self.store.arena.allocator);
        for (box.args) |scalar_id|
            try box_hint.append(self.scope.items[self.scope.items.len - 1 - scalar_id]);
        try box_hint.appendSlice(hint);
        const set_type = try self.analyzeDef(box.set_id, box_hint.items);
        switch (set_type) {
            .None => return type_.SetType{ .None = {} },
            .Concrete => |concrete| return type_.SetType{ .Concrete = .{
                .abstract_arity = if (concrete.abstract_arity > box.args.len) concrete.abstract_arity - box.args.len else 0,
                .columns = set_type.Concrete.columns[box.args.len..],
            } },
        }
    }

    fn analyzeExpr(self: *Analyzer, expr: *const core.Expr, hint: []const type_.ScalarType) Error!type_.SetType {
        try self.interrupter.check();
        switch (expr.*) {
            .None => {
                return type_.SetType{ .None = {} };
            },
            .Some => {
                return type_.SetType{
                    .Concrete = .{
                        .abstract_arity = 0,
                        .columns = &[0]type_.ScalarType{},
                    },
                };
            },
            .Scalar => |scalar| {
                const scalar_type: type_.ScalarType = switch (scalar) {
                    .Text => .Text,
                    .Number => .Number,
                    // TODO distinguish between literals and scalars
                    .Box => imp_panic("Shouldn't be any box literals", .{}),
                };
                return type_.SetType{
                    .Concrete = .{
                        .abstract_arity = 0,
                        .columns = try self.dupeScalars(&[1]type_.ScalarType{scalar_type}),
                    },
                };
            },
            .Union, .Intersect => |pair| {
                const left = try self.analyzeExpr(pair.left, hint);
                const right = try self.analyzeExpr(pair.right, hint);
                if (left == .None) return if (expr.* == .Union) right else .None;
                if (right == .None) return if (expr.* == .Union) left else .None;
                if (left.Concrete.columns.len != right.Concrete.columns.len) {
                    return self.setError(expr, "Mismatched arities: {} vs {}", .{ left.Concrete.columns.len, right.Concrete.columns.len });
                }
                const abstract_arity = max(left.Concrete.abstract_arity, right.Concrete.abstract_arity);
                var columns = try self.store.arena.allocator.alloc(type_.ScalarType, left.Concrete.columns.len);
                for (left.Concrete.columns) |left_type, i| {
                    try self.interrupter.check();
                    const right_type = right.Concrete.columns[i];
                    columns[i] = switch (expr.*) {
                        .Union => try self.unionScalar(expr, left_type, right_type),
                        .Intersect => try self.intersectScalar(expr, left_type, right_type),
                        else => unreachable,
                    };
                }
                return type_.SetType{
                    .Concrete = .{
                        .abstract_arity = abstract_arity,
                        .columns = columns,
                    },
                };
            },
            .Product => |pair| {
                const left = try self.analyzeExpr(pair.left, hint);
                if (left == .None) return type_.SetType{ .None = {} };
                const right = try self.analyzeExpr(pair.right, hint[min(hint.len, left.Concrete.columns.len)..]);
                if (right == .None) return type_.SetType{ .None = {} };
                const abstract_arity = if (right.Concrete.abstract_arity > 0)
                    left.Concrete.columns.len + right.Concrete.abstract_arity
                else
                    left.Concrete.abstract_arity;
                var columns = try self.store.arena.allocator.alloc(type_.ScalarType, left.Concrete.columns.len + right.Concrete.columns.len);
                var i: usize = 0;
                for (left.Concrete.columns) |left_type| {
                    columns[i] = left_type;
                    i += 1;
                }
                for (right.Concrete.columns) |right_type| {
                    columns[i] = right_type;
                    i += 1;
                }
                return type_.SetType{
                    .Concrete = .{
                        .abstract_arity = abstract_arity,
                        .columns = columns,
                    },
                };
            },
            .Equal => |pair| {
                // the hint for expr doesn't tell us anything about left or right
                const left = try self.analyzeExpr(pair.left, &[0]type_.ScalarType{});
                const right = try self.analyzeExpr(pair.right, &[0]type_.ScalarType{});
                if (!left.isFinite() or !right.isFinite()) {
                    return self.setError(expr, "Cannot equal one or more maybe-infinite sets: {} = {}", .{ left, right });
                }
                if (left == .Concrete and right == .Concrete) {
                    if (left.Concrete.columns.len != right.Concrete.columns.len) {
                        return self.setError(expr, "Mismatched arities: {} vs {}", .{ left.Concrete.columns.len, right.Concrete.columns.len });
                    }
                    for (left.Concrete.columns) |scalar_type, i| {
                        try self.interrupter.check();
                        _ = try self.intersectScalar(expr, scalar_type, right.Concrete.columns[i]);
                    }
                }
                return type_.SetType{
                    .Concrete = .{
                        .abstract_arity = 0,
                        .columns = &[0]type_.ScalarType{},
                    },
                };
            },
            .ScalarId => |scalar_id| {
                const scalar_type = self.scope.items[self.scope.items.len - 1 - scalar_id];
                return type_.SetType{
                    .Concrete = .{
                        .abstract_arity = 0,
                        .columns = try self.dupeScalars(&[1]type_.ScalarType{scalar_type}),
                    },
                };
            },
            .UnboxScalarId => |scalar_id| {
                const scalar_type = self.scope.items[self.scope.items.len - 1 - scalar_id];
                switch (scalar_type) {
                    .Box => |box| {
                        switch (box) {
                            .Normal => |normal| {
                                const box_hint = try std.mem.concat(
                                    &self.store.arena.allocator,
                                    type_.ScalarType,
                                    &[_][]const type_.ScalarType{
                                        normal.args,
                                        hint,
                                    },
                                );
                                const set_type = try self.analyzeDef(normal.set_id, box_hint);
                                switch (set_type) {
                                    .None => return type_.SetType{ .None = {} },
                                    .Concrete => |concrete| return type_.SetType{ .Concrete = .{
                                        .abstract_arity = if (concrete.abstract_arity > normal.args.len) concrete.abstract_arity - normal.args.len else 0,
                                        .columns = set_type.Concrete.columns[normal.args.len..],
                                    } },
                                }
                            },
                            .FixOrReduce => |fix_or_reduce| {
                                return fix_or_reduce.set_type;
                            },
                        }
                    },
                    else => {
                        return self.setError(expr, "Don't know what type will result from unboxing type {}", .{scalar_type});
                    },
                }
            },
            .SetId => |set_id| return self.analyzeDef(set_id, hint),
            .Negate => |body| {
                // the hint for expr doesn't tell us anything about body
                const body_type = try self.analyzeExpr(body, &[0]type_.ScalarType{});
                if (!body_type.isFinite()) {
                    return self.setError(expr, "The body of `!` must have finite type, found {}", .{body_type});
                }
                return type_.SetType{
                    .Concrete = .{
                        .abstract_arity = 0,
                        .columns = &[0]type_.ScalarType{},
                    },
                };
            },
            .Then => |then| {
                // the hint for expr doesn't tell us anything about condition
                const condition_type = try self.analyzeExpr(then.condition, &[0]type_.ScalarType{});
                if (!((condition_type == .None) or (condition_type == .Concrete and condition_type.Concrete.columns.len == 0))) {
                    return self.setError(expr, "The condition of `then` must have type `maybe`, found {}", .{condition_type});
                }
                return try self.analyzeExpr(then.true_branch, hint);
            },
            .Abstract => |body| {
                if (hint.len == 0) {
                    return self.setError(expr, "Could not infer the type of abstract arg", .{});
                } else {
                    // if we have a hint we can use it to specialize the body
                    try self.scope.append(hint[0]);
                    defer _ = self.scope.pop();
                    const body_type = try self.analyzeExpr(body, hint[1..]);
                    switch (body_type) {
                        .None => return type_.SetType{ .None = {} },
                        .Concrete => |concrete| {
                            const abstract_arity = concrete.abstract_arity + 1;
                            var columns = try ArrayList(type_.ScalarType).initCapacity(&self.store.arena.allocator, 1 + concrete.columns.len);
                            try columns.append(hint[0]);
                            try columns.appendSlice(concrete.columns);
                            return type_.SetType{
                                .Concrete = .{
                                    .abstract_arity = abstract_arity,
                                    .columns = columns.items,
                                },
                            };
                        },
                    }
                }
            },
            .Apply => |pair| {
                // can't make use of hint until we know which side is finite
                if (self.analyzeExpr(pair.left, &[0]type_.ScalarType{})) |left_type| {
                    return self.analyzeApply(expr, left_type, pair.right, hint);
                } else |_| {
                    // error might have been from lack of hints, so try other way around
                    // TODO could this cause exponential retries in large program?
                    if (self.analyzeExpr(pair.right, &[0]type_.ScalarType{})) |right_type| {
                        return self.analyzeApply(expr, right_type, pair.left, hint);
                    } else |err| {
                        return err;
                    }
                }
            },
            .Box => |box| {
                const args = try self.store.arena.allocator.alloc(type_.ScalarType, box.args.len);
                for (args) |*arg, i|
                    arg.* = self.scope.items[self.scope.items.len - 1 - box.args[i]];
                const box_type = type_.ScalarType{
                    .Box = .{
                        .Normal = .{
                            .set_id = box.set_id,
                            .args = args,
                        },
                    },
                };
                return type_.SetType{
                    .Concrete = .{
                        .abstract_arity = 0,
                        .columns = try self.dupeScalars(&[1]type_.ScalarType{box_type}),
                    },
                };
            },
            .Fix => |fix| {
                const init_type = try self.analyzeExpr(fix.init, &[0]type_.ScalarType{});
                if (!init_type.isFinite()) {
                    return self.setError(expr, "The initial value for fix must have finite type, found {}", .{init_type});
                }
                var fix_hint = try self.store.arena.allocator.alloc(type_.ScalarType, 1);
                var fix_type = init_type;
                var fix_box_type = type_.BoxType{ .FixOrReduce = .{
                    .set_id = fix.next.set_id,
                    .set_type = init_type,
                } };
                // TODO is there any case where fix could take more than 1 iteration to converge?
                //      could we just test that body_type.Concrete.columns[1] == init_type?
                //      what about case where init_type is none? require a hint?
                var max_iterations: usize = 100;
                while (max_iterations > 0) : (max_iterations -= 1) {
                    try self.interrupter.check();
                    fix_hint[0] = .{ .Box = fix_box_type };
                    const next_type = try self.analyzeBox(fix.next, fix_hint);
                    if (next_type == .None) {
                        return type_.SetType{ .None = {} };
                    }
                    // next should look like `?@prev ...`
                    if (next_type == .Concrete and next_type.Concrete.abstract_arity > 1) {
                        return self.setError(expr, "The body for fix must have finite type, found {}", .{next_type});
                    }
                    if (next_type.Concrete.columns.len < 1) {
                        return self.setError(expr, "The `next` argument for fix must have arity >= 1", .{});
                    }
                    if (next_type.Concrete.columns[0] != .Box) {
                        return self.setError(expr, "The `next` argument for fix must take a box as it's first argument. Found {}", .{next_type});
                    }
                    // drop the type for `prev`
                    const fix_columns = next_type.Concrete.columns[1..];
                    if (fix_type == .None) {
                        fix_type = .{ .Concrete = .{
                            .abstract_arity = 0,
                            .columns = try std.mem.dupe(&self.store.arena.allocator, type_.ScalarType, fix_columns),
                        } };
                    } else {
                        if (meta.deepEqual(fix_type.Concrete.columns, fix_columns)) {
                            // reached fixpoint
                            // TODO check that fix_box_type doesn't escape
                            return fix_type;
                        }
                        if (fix_type.Concrete.columns.len != fix_columns.len) {
                            return self.setError(expr, "The `next` argument for fix must have constant arity, changed from {} to {}", .{ fix_type.Concrete.columns.len, fix_columns.len });
                        }
                        var columns = try self.store.arena.allocator.alloc(type_.ScalarType, fix_type.Concrete.columns.len);
                        for (fix_columns) |column, i| {
                            columns[i] = try self.unionScalar(expr, fix_type.Concrete.columns[i], column);
                        }
                        fix_type.Concrete.columns = columns;
                    }
                    fix_box_type.FixOrReduce.set_type = fix_type;
                }
                return self.setError(expr, "Type of fix failed to converge, reached {}", .{fix_type});
            },
            .Reduce => |reduce| {
                const input_type = try self.analyzeExpr(reduce.input, &[0]type_.ScalarType{});
                if (!input_type.isFinite()) {
                    return self.setError(expr, "The input for reduce must have finite type, found {}", .{input_type});
                }
                const input_box_type = type_.BoxType{ .FixOrReduce = .{
                    .set_id = reduce.next.set_id,
                    .set_type = input_type,
                } };
                const init_type = try self.analyzeExpr(reduce.init, &[0]type_.ScalarType{});
                if (!init_type.isFinite()) {
                    return self.setError(expr, "The initial value for reduce must have finite type, found {}", .{init_type});
                }
                var reduce_hint = try self.store.arena.allocator.alloc(type_.ScalarType, 2);
                var reduce_type = init_type;
                var reduce_box_type = type_.BoxType{
                    .FixOrReduce = .{
                        .set_id = reduce.next.set_id,
                        .set_type = init_type,
                    },
                };
                var max_iterations: usize = 100;
                while (max_iterations > 0) : (max_iterations -= 1) {
                    try self.interrupter.check();
                    reduce_hint[0] = .{ .Box = reduce_box_type };
                    reduce_hint[1] = .{ .Box = input_box_type };
                    const next_type = try self.analyzeBox(reduce.next, reduce_hint);
                    if (next_type == .None) {
                        return type_.SetType{ .None = {} };
                    }
                    // next should look like `?@prev ?@input ...`
                    if (next_type.Concrete.abstract_arity > 2) {
                        return self.setError(expr, "The body for reduce must have finite type, found {}", .{next_type});
                    }
                    if (next_type.Concrete.columns.len < 2) {
                        return self.setError(expr, "The body for reduce must have arity >= 2", .{});
                    }
                    if (next_type.Concrete.columns[0] != .Box or next_type.Concrete.columns[1] != .Box) {
                        return self.setError(expr, "The body for reduce must take boxes as it's first two arguments. Found {}", .{next_type});
                    }
                    // drop the types for `prev` and `input`
                    const reduce_columns = next_type.Concrete.columns[2..];
                    if (reduce_type == .None) {
                        reduce_type = .{ .Concrete = .{
                            .abstract_arity = 0,
                            .columns = try std.mem.dupe(&self.store.arena.allocator, type_.ScalarType, reduce_columns),
                        } };
                    } else {
                        if (meta.deepEqual(reduce_type.Concrete.columns, reduce_columns)) {
                            // reached fixpoint
                            // TODO check that input_box_type and reduce_box_type do not escape
                            return reduce_type;
                        }
                        if (reduce_type.Concrete.columns.len != reduce_columns.len) {
                            return self.setError(expr, "The body for reduce must have constant arity, changed from {} to {}", .{ reduce_type.Concrete.columns.len, reduce_columns.len });
                        }
                        var columns = try self.store.arena.allocator.alloc(type_.ScalarType, reduce_type.Concrete.columns.len);
                        for (reduce_columns) |column, i| {
                            try self.interrupter.check();
                            columns[i] = try self.unionScalar(expr, reduce_type.Concrete.columns[i], column);
                        }
                        reduce_type.Concrete.columns = columns;
                    }
                    reduce_box_type.FixOrReduce.set_type = reduce_type;
                }
                return self.setError(expr, "Type of reduce failed to converge, reached {}", .{reduce_type});
            },
            .Enumerate => |body| {
                const body_type = try self.analyzeExpr(body, &[0]type_.ScalarType{});
                if (body_type == .None) {
                    return type_.SetType{ .None = {} };
                }
                if (!body_type.isFinite()) {
                    return self.setError(expr, "The body of `enumerate` must have finite type, found {}", .{body_type});
                }
                var columns = try ArrayList(type_.ScalarType).initCapacity(&self.store.arena.allocator, 1 + body_type.Concrete.columns.len);
                try columns.append(.Number);
                try columns.appendSlice(body_type.Concrete.columns);
                return type_.SetType{
                    .Concrete = .{
                        .abstract_arity = 0,
                        .columns = columns.items,
                    },
                };
            },
            .Annotate => |annotate| {
                // TODO some annotations affect types eg solve
                return try self.analyzeExpr(annotate.body, hint);
            },
            .Native => |native| {
                return type_.SetType{
                    .Concrete = switch (native) {
                        .Add, .Subtract, .Multiply, .Divide, .Modulus, .Range => .{
                            .abstract_arity = 2,
                            .columns = try self.dupeScalars(&[3]type_.ScalarType{ .Number, .Number, .Number }),
                        },
                        .GreaterThan, .GreaterThanOrEqual => .{
                            .abstract_arity = 2,
                            .columns = try self.dupeScalars(&[2]type_.ScalarType{ .Number, .Number }),
                        },
                    },
                };
            },
        }
    }

    fn analyzeApply(self: *Analyzer, expr: *const core.Expr, left_type: type_.SetType, right_expr: *const core.Expr, hint: []const type_.ScalarType) Error!type_.SetType {
        if (left_type == .None) return type_.SetType{ .None = {} };
        const right_hint = try std.mem.concat(
            &self.store.arena.allocator,
            type_.ScalarType,
            &[_][]const type_.ScalarType{
                left_type.Concrete.columns,
                hint,
            },
        );
        const right_type = try self.analyzeExpr(right_expr, right_hint);
        if (right_type == .None) return type_.SetType{ .None = {} };
        if (!left_type.isFinite() and !right_type.isFinite())
            return self.setError(expr, "Cannot apply two maybe-infinite sets: {} applied to {}", .{ left_type, right_type });
        const joined_arity = min(left_type.Concrete.columns.len, right_type.Concrete.columns.len);
        for (left_type.Concrete.columns[0..joined_arity]) |column, i| {
            try self.interrupter.check();
            _ = try self.intersectScalar(expr, column, right_type.Concrete.columns[i]);
        }
        const prev_abstract_arity = max(left_type.Concrete.abstract_arity, right_type.Concrete.abstract_arity);
        const abstract_arity = if (prev_abstract_arity > joined_arity)
            prev_abstract_arity - joined_arity
        else
            0;
        const columns = if (left_type.Concrete.columns.len > right_type.Concrete.columns.len)
            left_type.Concrete.columns[joined_arity..]
        else
            right_type.Concrete.columns[joined_arity..];
        return type_.SetType{
            .Concrete = .{
                .abstract_arity = abstract_arity,
                .columns = columns,
            },
        };
    }

    fn dupeScalars(self: *Analyzer, scope: []const type_.ScalarType) Error![]type_.ScalarType {
        return std.mem.dupe(&self.store.arena.allocator, type_.ScalarType, scope);
    }

    fn unionScalar(self: *Analyzer, parent_expr: *const core.Expr, a: type_.ScalarType, b: type_.ScalarType) Error!type_.ScalarType {
        if (meta.deepEqual(a, b)) {
            return a;
        } else {
            return self.setError(parent_expr, "TODO type unions are not implemented yet: {} | {}", .{ a, b });
        }
    }

    fn intersectScalar(self: *Analyzer, parent_expr: *const core.Expr, a: type_.ScalarType, b: type_.ScalarType) Error!type_.ScalarType {
        if (meta.deepEqual(a, b)) {
            return a;
        } else {
            return self.setError(parent_expr, "Intersection of {} and {} is empty", .{ a, b });
        }
    }
};
