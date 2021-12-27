const std = @import("std");
const imp = @import("../../../imp.zig");
const u = imp.util;
const core = imp.lang.repr.core;
const type_ = imp.lang.repr.type_;

pub fn analyze(
    arena: *u.ArenaAllocator,
    core_program: core.Program,
    interrupter: imp.lang.Interrupter,
    error_info: *?ErrorInfo,
) Error!type_.ProgramType {
    var defs = try arena.allocator().alloc(u.ArrayList(type_.Specialization), core_program.defs.len);
    for (defs) |*def|
        def.* = u.ArrayList(type_.Specialization).init(arena.allocator());
    var analyzer = Analyzer{
        .arena = arena,
        .core_program = core_program,
        .defs = defs,
        .scope = u.ArrayList(type_.ScalarType).init(arena.allocator()),
        .interrupter = interrupter,
        .error_info = error_info,
    };
    const program_type = try analyzer.analyzeDef(.{ .id = core_program.defs.len - 1 }, &.{});
    return type_.ProgramType{
        .defs = analyzer.defs,
        .program_type = program_type,
    };
}

pub const Error = error{
    // sets error_info
    AnalyzeError,

    // does not set error_info
    OutOfMemory,
    WasInterrupted,
};

pub const ErrorInfo = struct {
    expr_id: core.ExprId,
    // TODO want something like a stack of specializations
    //callstack: []const struct {
    //    def_id: DefId,
    //    hint: type_.TupleType,
    //},
    message: []const u8,
    kind: ErrorKind = .Other,
};

pub const ErrorKind = enum {
    NoHintForArg,
    Other,
};

// --------------------------------------------------------------------------------

pub const Analyzer = struct {
    arena: *u.ArenaAllocator,
    core_program: core.Program,
    defs: []u.ArrayList(type_.Specialization),
    // TODO maybe scope should be passed as an arg like hint
    scope: u.ArrayList(type_.ScalarType),
    interrupter: imp.lang.Interrupter,
    error_info: *?ErrorInfo,

    fn setError(self: *Analyzer, expr_id: core.ExprId, comptime fmt: []const u8, args: anytype, kind: ErrorKind) Error {
        const message = try u.formatToString(self.arena.allocator(), fmt, args);
        self.error_info.* = ErrorInfo{
            .expr_id = expr_id,
            .message = message,
            .kind = kind,
        };
        return error.AnalyzeError;
    }

    fn analyzeDef(self: *Analyzer, def_id: core.DefId, hint: type_.TupleType) Error!type_.SetType {
        const type_union = &self.defs[def_id.id];

        // see if we already analyzed this specialization
        for (type_union.items) |specialization| {
            // if could specialize with a shorter hint, can specialize with this one
            if (specialization.hint.len <= hint.len and
                u.deepEqual(specialization.hint, hint[0..specialization.hint.len]))
                return specialization.set_type;
        }

        // otherwise, analyze
        const old_scope = self.scope;
        defer self.scope = old_scope;
        self.scope = u.ArrayList(type_.ScalarType).init(self.arena.allocator());
        const set_type = try self.analyzeExpr(self.core_program.defs[def_id.id], hint);

        // memoize result
        const used_hint = switch (set_type) {
            // always empty so can't say how much of the hint was used
            .None => hint,
            // specialized, so cannot have used >concrete.columns.len of the hint
            .Concrete => |concrete| hint[0..u.min(hint.len, concrete.columns.len)],
        };
        try self.defs[def_id.id].append(.{
            .hint = used_hint,
            .set_type = set_type,
        });

        return set_type;
    }

    fn analyzeBox(self: *Analyzer, box: core.Box, hint: type_.TupleType) Error!type_.SetType {
        var box_hint = u.ArrayList(type_.ScalarType).init(self.arena.allocator());
        for (box.args) |scalar_id|
            try box_hint.append(self.scope.items[self.scope.items.len - 1 - scalar_id.id]);
        try box_hint.appendSlice(hint);
        const set_type = try self.analyzeDef(box.def_id, box_hint.items);
        switch (set_type) {
            .None => return type_.SetType{ .None = {} },
            .Concrete => |concrete| return type_.SetType{ .Concrete = .{
                .abstract_arity = if (concrete.abstract_arity > box.args.len) concrete.abstract_arity - box.args.len else 0,
                .columns = set_type.Concrete.columns[box.args.len..],
            } },
        }
    }

    fn analyzeExpr(self: *Analyzer, expr_id: core.ExprId, hint: type_.TupleType) Error!type_.SetType {
        try self.interrupter.check();
        const expr = self.core_program.exprs[expr_id.id];
        switch (expr) {
            .None => {
                return type_.SetType{ .None = {} };
            },
            .Some => {
                return type_.SetType{
                    .Concrete = .{
                        .abstract_arity = 0,
                        .columns = &.{},
                    },
                };
            },
            .Scalar => |scalar| {
                const scalar_type: type_.ScalarType = switch (scalar) {
                    .Text => .Text,
                    .Number => .Number,
                    .Box => u.imp_panic("Shouldn't be any box literals", .{}),
                };
                return type_.SetType{
                    .Concrete = .{
                        .abstract_arity = 0,
                        .columns = try self.dupeScalars(&.{scalar_type}),
                    },
                };
            },
            .Union, .Intersect => |pair| {
                const left = try self.analyzeExpr(pair.left, hint);
                const right = try self.analyzeExpr(pair.right, hint);
                if (left == .None) return if (expr == .Union) right else .None;
                if (right == .None) return if (expr == .Union) left else .None;
                if (left.Concrete.columns.len != right.Concrete.columns.len) {
                    return self.setError(expr_id, "Mismatched arities: {} vs {}", .{ left.Concrete.columns.len, right.Concrete.columns.len }, .Other);
                }
                const abstract_arity = u.max(left.Concrete.abstract_arity, right.Concrete.abstract_arity);
                var columns = try self.arena.allocator().alloc(type_.ScalarType, left.Concrete.columns.len);
                for (left.Concrete.columns) |left_type, i| {
                    try self.interrupter.check();
                    const right_type = right.Concrete.columns[i];
                    columns[i] = switch (expr) {
                        .Union => try self.unionScalar(expr_id, left_type, right_type),
                        .Intersect => try self.intersectScalar(expr_id, left_type, right_type),
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
                const right = try self.analyzeExpr(pair.right, hint[u.min(hint.len, left.Concrete.columns.len)..]);
                if (right == .None) return type_.SetType{ .None = {} };
                const abstract_arity = if (right.Concrete.abstract_arity > 0)
                    left.Concrete.columns.len + right.Concrete.abstract_arity
                else
                    left.Concrete.abstract_arity;
                var columns = try self.arena.allocator().alloc(type_.ScalarType, left.Concrete.columns.len + right.Concrete.columns.len);
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
                const left = try self.analyzeExpr(pair.left, &.{});
                const right = try self.analyzeExpr(pair.right, &.{});
                if (!left.isFinite() or !right.isFinite()) {
                    return self.setError(expr_id, "Cannot equal one or more maybe-infinite sets: {} = {}", .{ left, right }, .Other);
                }
                if (left == .Concrete and right == .Concrete) {
                    if (left.Concrete.columns.len != right.Concrete.columns.len) {
                        return self.setError(expr_id, "Mismatched arities: {} vs {}", .{ left.Concrete.columns.len, right.Concrete.columns.len }, .Other);
                    }
                    for (left.Concrete.columns) |scalar_type, i| {
                        try self.interrupter.check();
                        _ = try self.intersectScalar(expr_id, scalar_type, right.Concrete.columns[i]);
                    }
                }
                return type_.SetType{
                    .Concrete = .{
                        .abstract_arity = 0,
                        .columns = &.{},
                    },
                };
            },
            .ScalarId => |scalar_id| {
                const scalar_type = self.scope.items[self.scope.items.len - 1 - scalar_id.id];
                return type_.SetType{
                    .Concrete = .{
                        .abstract_arity = 0,
                        .columns = try self.dupeScalars(&.{scalar_type}),
                    },
                };
            },
            .UnboxScalarId => |scalar_id| {
                const scalar_type = self.scope.items[self.scope.items.len - 1 - scalar_id.id];
                switch (scalar_type) {
                    .Box => |box| {
                        switch (box) {
                            .Normal => |normal| {
                                const box_hint = try std.mem.concat(
                                    self.arena.allocator(),
                                    type_.ScalarType,
                                    &.{
                                        normal.args,
                                        hint,
                                    },
                                );
                                const set_type = try self.analyzeDef(normal.def_id, box_hint);
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
                        return self.setError(expr_id, "Don't know what type will result from unboxing type {}", .{scalar_type}, .Other);
                    },
                }
            },
            .DefId => |def_id| return self.analyzeDef(def_id, hint),
            .Negate => |body| {
                // the hint for expr doesn't tell us anything about body
                const body_type = try self.analyzeExpr(body, &.{});
                if (!body_type.isFinite()) {
                    return self.setError(expr_id, "The body of `!` must have finite type, found {}", .{body_type}, .Other);
                }
                return type_.SetType{
                    .Concrete = .{
                        .abstract_arity = 0,
                        .columns = &.{},
                    },
                };
            },
            .Then => |then| {
                // the hint for expr doesn't tell us anything about condition
                const condition_type = try self.analyzeExpr(then.condition, &.{});
                if (!((condition_type == .None) or (condition_type == .Concrete and condition_type.Concrete.columns.len == 0))) {
                    return self.setError(expr_id, "The condition of `then` must have type `maybe`, found {}", .{condition_type}, .Other);
                }
                return try self.analyzeExpr(then.true_branch, hint);
            },
            .Abstract => |body| {
                if (hint.len == 0) {
                    return self.setError(expr_id, "Could not infer the type of abstract arg", .{}, .NoHintForArg);
                } else {
                    // if we have a hint we can use it to specialize the body
                    try self.scope.append(hint[0]);
                    defer _ = self.scope.pop();
                    const body_type = try self.analyzeExpr(body, hint[1..]);
                    switch (body_type) {
                        .None => return type_.SetType{ .None = {} },
                        .Concrete => |concrete| {
                            const abstract_arity = concrete.abstract_arity + 1;
                            var columns = try u.ArrayList(type_.ScalarType).initCapacity(self.arena.allocator(), 1 + concrete.columns.len);
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
                if (self.analyzeExpr(pair.left, &.{})) |left_type| {
                    return self.analyzeApply(expr_id, left_type, pair.right, hint);
                } else |left_err| {
                    if (self.error_info.*.?.kind != .NoHintForArg) return left_err;
                    // error was from lack of hints, so try other way around
                    // TODO could this cause exponential retries in large program?
                    self.error_info.* = null;
                    if (self.analyzeExpr(pair.right, &.{})) |right_type| {
                        return self.analyzeApply(expr_id, right_type, pair.left, hint);
                    } else |right_err| {
                        if (self.error_info.*.?.kind != .NoHintForArg) return right_err;
                        return self.setError(expr_id, "Cannot apply two maybe-infinite sets", .{}, .Other);
                    }
                }
            },
            .Box => |box| {
                const args = try self.arena.allocator().alloc(type_.ScalarType, box.args.len);
                for (args) |*arg, i|
                    arg.* = self.scope.items[self.scope.items.len - 1 - box.args[i].id];
                const box_type = type_.ScalarType{
                    .Box = .{
                        .Normal = .{
                            .def_id = box.def_id,
                            .args = args,
                        },
                    },
                };
                return type_.SetType{
                    .Concrete = .{
                        .abstract_arity = 0,
                        .columns = try self.dupeScalars(&.{box_type}),
                    },
                };
            },
            .Fix => |fix| {
                const init_type = try self.analyzeExpr(fix.init, &.{});
                if (!init_type.isFinite()) {
                    return self.setError(expr_id, "The initial value for fix must have finite type, found {}", .{init_type}, .Other);
                }
                var fix_hint = try self.arena.allocator().alloc(type_.ScalarType, 1);
                var fix_type = init_type;
                var fix_box_type = type_.BoxType{ .FixOrReduce = .{
                    .def_id = fix.next.def_id,
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
                        return self.setError(expr_id, "The body for fix must have finite type, found {}", .{next_type}, .Other);
                    }
                    if (next_type.Concrete.columns.len < 1) {
                        return self.setError(expr_id, "The `next` argument for fix must have arity >= 1", .{}, .Other);
                    }
                    if (next_type.Concrete.columns[0] != .Box) {
                        return self.setError(expr_id, "The `next` argument for fix must take a box as it's first argument. Found {}", .{next_type}, .Other);
                    }
                    // drop the type for `prev`
                    const fix_columns = next_type.Concrete.columns[1..];
                    if (fix_type == .None) {
                        fix_type = .{ .Concrete = .{
                            .abstract_arity = 0,
                            .columns = try self.arena.allocator().dupe(type_.ScalarType, fix_columns),
                        } };
                    } else {
                        if (u.deepEqual(fix_type.Concrete.columns, fix_columns)) {
                            // reached fixpoint
                            // TODO check that fix_box_type doesn't escape
                            return fix_type;
                        }
                        if (fix_type.Concrete.columns.len != fix_columns.len) {
                            return self.setError(expr_id, "The `next` argument for fix must have constant arity, changed from {} to {}", .{ fix_type.Concrete.columns.len, fix_columns.len }, .Other);
                        }
                        var columns = try self.arena.allocator().alloc(type_.ScalarType, fix_type.Concrete.columns.len);
                        for (fix_columns) |column, i| {
                            columns[i] = try self.unionScalar(expr_id, fix_type.Concrete.columns[i], column);
                        }
                        fix_type.Concrete.columns = columns;
                    }
                    fix_box_type.FixOrReduce.set_type = fix_type;
                }
                return self.setError(expr_id, "Type of fix failed to converge, reached {}", .{fix_type}, .Other);
            },
            .Reduce => |reduce| {
                const input_type = try self.analyzeExpr(reduce.input, &.{});
                if (!input_type.isFinite()) {
                    return self.setError(expr_id, "The input for reduce must have finite type, found {}", .{input_type}, .Other);
                }
                const input_box_type = type_.BoxType{ .FixOrReduce = .{
                    .def_id = reduce.next.def_id,
                    .set_type = input_type,
                } };
                const init_type = try self.analyzeExpr(reduce.init, &.{});
                if (!init_type.isFinite()) {
                    return self.setError(expr_id, "The initial value for reduce must have finite type, found {}", .{init_type}, .Other);
                }
                var reduce_hint = try self.arena.allocator().alloc(type_.ScalarType, 2);
                var reduce_type = init_type;
                var reduce_box_type = type_.BoxType{
                    .FixOrReduce = .{
                        .def_id = reduce.next.def_id,
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
                        return self.setError(expr_id, "The body for reduce must have finite type, found {}", .{next_type}, .Other);
                    }
                    if (next_type.Concrete.columns.len < 2) {
                        return self.setError(expr_id, "The body for reduce must have arity >= 2", .{}, .Other);
                    }
                    if (next_type.Concrete.columns[0] != .Box or next_type.Concrete.columns[1] != .Box) {
                        return self.setError(expr_id, "The body for reduce must take boxes as it's first two arguments. Found {}", .{next_type}, .Other);
                    }
                    // drop the types for `prev` and `input`
                    const reduce_columns = next_type.Concrete.columns[2..];
                    if (reduce_type == .None) {
                        reduce_type = .{ .Concrete = .{
                            .abstract_arity = 0,
                            .columns = try self.arena.allocator().dupe(type_.ScalarType, reduce_columns),
                        } };
                    } else {
                        if (u.deepEqual(reduce_type.Concrete.columns, reduce_columns)) {
                            // reached fixpoint
                            // TODO check that input_box_type and reduce_box_type do not escape
                            return reduce_type;
                        }
                        if (reduce_type.Concrete.columns.len != reduce_columns.len) {
                            return self.setError(expr_id, "The body for reduce must have constant arity, changed from {} to {}", .{ reduce_type.Concrete.columns.len, reduce_columns.len }, .Other);
                        }
                        var columns = try self.arena.allocator().alloc(type_.ScalarType, reduce_type.Concrete.columns.len);
                        for (reduce_columns) |column, i| {
                            try self.interrupter.check();
                            columns[i] = try self.unionScalar(expr_id, reduce_type.Concrete.columns[i], column);
                        }
                        reduce_type.Concrete.columns = columns;
                    }
                    reduce_box_type.FixOrReduce.set_type = reduce_type;
                }
                return self.setError(expr_id, "Type of reduce failed to converge, reached {}", .{reduce_type}, .Other);
            },
            .Enumerate => |body| {
                const body_type = try self.analyzeExpr(body, &.{});
                if (body_type == .None) {
                    return type_.SetType{ .None = {} };
                }
                if (!body_type.isFinite()) {
                    return self.setError(expr_id, "The body of `enumerate` must have finite type, found {}", .{body_type}, .Other);
                }
                var columns = try u.ArrayList(type_.ScalarType).initCapacity(self.arena.allocator(), 1 + body_type.Concrete.columns.len);
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
            .Watch => |watch| return try self.analyzeExpr(watch.body, hint),
            .Native => |native| {
                return type_.SetType{
                    .Concrete = switch (native) {
                        .Add, .Subtract, .Multiply, .Divide, .Modulus, .Range => .{
                            .abstract_arity = 2,
                            .columns = try self.dupeScalars(&.{ .Number, .Number, .Number }),
                        },
                        .GreaterThan, .GreaterThanOrEqual => .{
                            .abstract_arity = 2,
                            .columns = try self.dupeScalars(&.{ .Number, .Number }),
                        },
                    },
                };
            },
        }
    }

    fn analyzeApply(self: *Analyzer, parent_expr_id: core.ExprId, left_type: type_.SetType, right_expr_id: core.ExprId, hint: type_.TupleType) Error!type_.SetType {
        if (left_type == .None) return type_.SetType{ .None = {} };
        const right_hint = try std.mem.concat(
            self.arena.allocator(),
            type_.ScalarType,
            &.{
                left_type.Concrete.columns,
                hint,
            },
        );
        const right_type = try self.analyzeExpr(right_expr_id, right_hint);
        if (right_type == .None) return type_.SetType{ .None = {} };
        if (!left_type.isFinite() and !right_type.isFinite())
            return self.setError(parent_expr_id, "Cannot apply two maybe-infinite sets: {} applied to {}", .{ left_type, right_type }, .Other);
        const joined_arity = u.min(left_type.Concrete.columns.len, right_type.Concrete.columns.len);
        for (left_type.Concrete.columns[0..joined_arity]) |column, i| {
            _ = try self.intersectScalar(parent_expr_id, column, right_type.Concrete.columns[i]);
        }
        const prev_abstract_arity = u.max(left_type.Concrete.abstract_arity, right_type.Concrete.abstract_arity);
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

    fn dupeScalars(self: *Analyzer, scope: type_.TupleType) Error![]type_.ScalarType {
        return self.arena.allocator().dupe(type_.ScalarType, scope);
    }

    fn unionScalar(self: *Analyzer, parent_expr_id: core.ExprId, a: type_.ScalarType, b: type_.ScalarType) Error!type_.ScalarType {
        if (u.deepEqual(a, b)) {
            return a;
        } else {
            return self.setError(parent_expr_id, "TODO type unions are not implemented yet: {} | {}", .{ a, b }, .Other);
        }
    }

    fn intersectScalar(self: *Analyzer, parent_expr_id: core.ExprId, a: type_.ScalarType, b: type_.ScalarType) Error!type_.ScalarType {
        if (u.deepEqual(a, b)) {
            return a;
        } else {
            return self.setError(parent_expr_id, "Intersection of {} and {} is empty", .{ a, b }, .Other);
        }
    }
};
