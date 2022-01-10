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
        .warnings = u.ArrayList(type_.Warning).init(arena.allocator()),
    };
    const program_type = try analyzer.analyzeDef(.{ .id = core_program.defs.len - 1 }, &.{}, .Apply);
    return type_.ProgramType{
        .defs = analyzer.defs,
        .program_type = program_type,
        .warnings = analyzer.warnings.toOwnedSlice(),
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
    warnings: u.ArrayList(type_.Warning),

    fn setError(self: *Analyzer, expr_id: core.ExprId, comptime fmt: []const u8, args: anytype, kind: ErrorKind) Error {
        const message = try u.formatToString(self.arena.allocator(), fmt, args);
        self.error_info.* = ErrorInfo{
            .expr_id = expr_id,
            .message = message,
            .kind = kind,
        };
        return error.AnalyzeError;
    }

    fn addWarning(self: *Analyzer, expr_id: core.ExprId, comptime fmt: []const u8, args: anytype) !void {
        const message = try u.formatToString(self.arena.allocator(), fmt, args);
        try self.warnings.append(.{ .expr_id = expr_id, .message = message });
    }

    fn analyzeDef(self: *Analyzer, def_id: core.DefId, hint: []const type_.ScalarType, hint_mode: type_.HintMode) Error!type_.SetType {
        const type_union = &self.defs[def_id.id];

        // see if we already analyzed this specialization
        for (type_union.items) |specialization| {
            // if could specialize with a shorter hint, can specialize with this one
            if (specialization.hint_mode == hint_mode and
                specialization.hint.len <= hint.len and
                u.deepEqual(specialization.hint, hint[0..specialization.hint.len]))
                return specialization.set_type;
        }

        // otherwise, analyze
        const old_scope = self.scope;
        defer self.scope = old_scope;
        self.scope = u.ArrayList(type_.ScalarType).init(self.arena.allocator());
        const set_type = try self.analyzeExpr(self.core_program.defs[def_id.id], hint, hint_mode);

        // memoize result
        var used_hint_ix: ?usize = null;
        var iter = set_type.row_types.keyIterator();
        while (iter.next()) |row_type| {
            used_hint_ix = u.max(used_hint_ix orelse 0, row_type.columns.len);
        }
        const used_hint = hint[0..u.min(hint.len, used_hint_ix orelse 0)];
        try self.defs[def_id.id].append(.{
            .hint = try self.arena.allocator().dupe(type_.ScalarType, used_hint),
            .hint_mode = hint_mode,
            .set_type = set_type,
        });

        return set_type;
    }

    fn analyzeBox(self: *Analyzer, box: core.Box, hint: []const type_.ScalarType, hint_mode: type_.HintMode) Error!type_.SetType {
        var box_hint = u.ArrayList(type_.ScalarType).init(self.arena.allocator());
        for (box.args) |scalar_id|
            try box_hint.append(self.scope.items[self.scope.items.len - 1 - scalar_id.id]);
        try box_hint.appendSlice(hint);
        const set_type = try self.analyzeDef(box.def_id, box_hint.items, hint_mode);
        var row_types = u.DeepHashSet(type_.RowType).init(self.arena.allocator());
        var row_types_iter = set_type.row_types.keyIterator();
        while (row_types_iter.next()) |row_type| {
            try row_types.put(.{ .columns = row_type.columns[box.args.len..] }, {});
        }
        return type_.SetType{ .row_types = row_types };
    }

    fn analyzeExpr(self: *Analyzer, expr_id: core.ExprId, hint: []const type_.ScalarType, hint_mode: type_.HintMode) Error!type_.SetType {
        const set_type = try self.analyzeExprInner(expr_id, hint, hint_mode);
        return set_type;
    }

    fn analyzeExprInner(self: *Analyzer, expr_id: core.ExprId, hint: []const type_.ScalarType, hint_mode: type_.HintMode) Error!type_.SetType {
        try self.interrupter.check();
        const expr = self.core_program.exprs[expr_id.id];
        switch (expr) {
            .None => {
                return type_.SetType.none(self.arena.allocator());
            },
            .Some => {
                return type_.SetType.some(self.arena.allocator());
            },
            .Scalar => |scalar| {
                const scalar_type: type_.ScalarType = switch (scalar) {
                    .Text => .Text,
                    .Number => .Number,
                    .Box => u.imp_panic("Shouldn't be any box literals", .{}),
                    .TextTag => |text| .{ .TextTag = text },
                    .NumberTag => |number| .{ .NumberTag = number },
                };
                return type_.SetType.fromScalar(self.arena.allocator(), scalar_type);
            },
            .Union => |pair| {
                var row_types = u.DeepHashSet(type_.RowType).init(self.arena.allocator());
                const left = try self.analyzeExpr(pair.left, hint, hint_mode);
                const right = try self.analyzeExpr(pair.right, hint, hint_mode);
                var left_iter = left.row_types.keyIterator();
                while (left_iter.next()) |left_row_type| {
                    try row_types.put(left_row_type.*, {});
                }
                var right_iter = right.row_types.keyIterator();
                while (right_iter.next()) |right_row_type| {
                    try row_types.put(right_row_type.*, {});
                }
                return type_.SetType{ .row_types = row_types };
            },
            .Intersect => |pair| {
                if (self.analyzeExpr(pair.left, hint, hint_mode)) |left| {
                    return self.analyzeIntersect(expr_id, left, pair.right, false);
                } else |left_err| {
                    if (self.error_info.*.?.kind != .NoHintForArg)
                        return left_err;
                    self.error_info.* = null;
                }

                // error was from lack of hints, so try other way around
                if (self.analyzeExpr(pair.right, hint, hint_mode)) |right| {
                    return self.analyzeIntersect(expr_id, right, pair.left, true);
                } else |right_err| {
                    if (self.error_info.*.?.kind != .NoHintForArg)
                        return right_err;
                    self.error_info.* = null;
                }

                return self.setError(expr_id, "Cannot intersect two maybe-infinite sets", .{}, .Other);
            },
            .Product => |pair| {
                var row_types = u.DeepHashSet(type_.RowType).init(self.arena.allocator());
                const left = try self.analyzeExpr(pair.left, hint, hint_mode);
                var left_iter = left.row_types.keyIterator();
                while (left_iter.next()) |left_row_type| {
                    const right = try self.analyzeExpr(pair.right, hint[u.min(hint.len, left_row_type.columns.len)..], hint_mode);
                    var right_iter = right.row_types.keyIterator();
                    while (right_iter.next()) |right_row_type| {
                        const columns_inputs = [2][]const type_.ScalarType{
                            left_row_type.columns,
                            right_row_type.columns,
                        };
                        const columns = try std.mem.concat(self.arena.allocator(), type_.ScalarType, &columns_inputs);
                        try row_types.put(.{ .columns = columns }, {});
                    }
                }
                return type_.SetType{ .row_types = row_types };
            },
            .Equal => |pair| {
                // the hint for expr doesn't tell us anything about left or right
                const left = try self.analyzeExpr(pair.left, &.{}, .Apply);
                const right = try self.analyzeExpr(pair.right, &.{}, .Apply);
                var is_intersection_empty = true;
                var left_iter = left.row_types.keyIterator();
                while (left_iter.next()) |left_row_type| {
                    if (right.row_types.contains(left_row_type.*)) {
                        is_intersection_empty = false;
                    }
                }
                if (is_intersection_empty and left.row_types.count() > 0 and right.row_types.count() > 0)
                    try self.addWarning(expr_id, "Will never be equal: {} vs {}", .{ left, right });
                return type_.SetType.some(self.arena.allocator());
            },
            .ScalarId => |scalar_id| {
                const scalar_type = self.scope.items[self.scope.items.len - 1 - scalar_id.id];
                return type_.SetType.fromScalar(self.arena.allocator(), scalar_type);
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
                                const set_type = try self.analyzeDef(normal.def_id, box_hint, hint_mode);
                                var row_types = u.DeepHashSet(type_.RowType).init(self.arena.allocator());
                                var row_types_iter = set_type.row_types.keyIterator();
                                while (row_types_iter.next()) |row_type| {
                                    try row_types.put(.{ .columns = row_type.columns[normal.args.len..] }, {});
                                }
                                return type_.SetType{ .row_types = row_types };
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
            .DefId => |def_id| return self.analyzeDef(def_id, hint, hint_mode),
            .Negate => |body| {
                // the hint for expr doesn't tell us anything about body
                _ = try self.analyzeExpr(body, &.{}, .Apply);
                return try type_.SetType.some(self.arena.allocator());
            },
            .Then => |then| {
                // the hint for expr doesn't tell us anything about condition
                const condition_type = try self.analyzeExpr(then.condition, &.{}, .Apply);
                if (!condition_type.isBoolish()) {
                    try self.addWarning(expr_id, "The condition of `then` should have type `maybe`, found {}", .{condition_type});
                }
                return if (condition_type.row_types.count() == 0)
                    type_.SetType.none(self.arena.allocator())
                else
                    try self.analyzeExpr(then.true_branch, hint, hint_mode);
            },
            .Abstract => |body| {
                if (hint.len == 0) {
                    return switch (hint_mode) {
                        .Apply => self.setError(expr_id, "Could not infer the type of abstract arg", .{}, .NoHintForArg),
                        .Intersect => type_.SetType.none(self.arena.allocator()),
                    };
                } else {
                    // if we have a hint we can use it to specialize the body
                    try self.scope.append(hint[0]);
                    defer _ = self.scope.pop();
                    const body_type = try self.analyzeExpr(body, hint[1..], hint_mode);
                    var row_types = u.DeepHashSet(type_.RowType).init(self.arena.allocator());
                    var body_iter = body_type.row_types.keyIterator();
                    while (body_iter.next()) |body_row_type| {
                        const columns = try std.mem.concat(self.arena.allocator(), type_.ScalarType, &.{
                            &[_]type_.ScalarType{hint[0]},
                            body_row_type.columns,
                        });
                        try row_types.put(.{ .columns = columns }, {});
                    }
                    return type_.SetType{ .row_types = row_types };
                }
            },
            .Apply => |pair| {
                // can't make use of hint until we know which side is finite
                if (self.analyzeExpr(pair.left, &.{}, .Apply)) |left_type| {
                    if (self.analyzeExpr(pair.right, &.{}, .Apply)) |right_type| {
                        return self.analyzeApplyFinite(expr_id, left_type, right_type);
                    } else |right_err| {
                        if (self.error_info.*.?.kind != .NoHintForArg)
                            return right_err;
                        self.error_info.* = null;
                        return self.analyzeApplyNonfinite(expr_id, left_type, pair.right, hint, hint_mode, false);
                    }
                } else |left_err| {
                    if (self.error_info.*.?.kind != .NoHintForArg)
                        return left_err;
                    self.error_info.* = null;
                }

                // error was from lack of hints, so try other way around
                // TODO could this cause exponential retries in large program?
                if (self.analyzeExpr(pair.right, &.{}, .Apply)) |right_type| {
                    return self.analyzeApplyNonfinite(expr_id, right_type, pair.left, hint, hint_mode, true);
                } else |right_err| {
                    if (self.error_info.*.?.kind != .NoHintForArg) return right_err;
                }

                return self.setError(expr_id, "Cannot apply two maybe-infinite sets", .{}, .Other);
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
                return type_.SetType.fromScalar(self.arena.allocator(), box_type);
            },
            .Fix => |fix| {
                const init_type = try self.analyzeExpr(fix.init, &.{}, .Apply);
                var fix_hint = try self.arena.allocator().alloc(type_.ScalarType, 1);
                var fix_type = init_type;
                var fix_box_type = type_.BoxType{ .FixOrReduce = .{
                    .def_id = fix.next.def_id,
                    .set_type = init_type,
                } };
                // TODO is there any case where fix could take more than 1 iteration to converge?
                var max_iterations: usize = 10;
                while (max_iterations > 0) : (max_iterations -= 1) {
                    try self.interrupter.check();
                    fix_hint[0] = .{ .Box = fix_box_type };
                    var new_fix_row_types = try fix_type.row_types.clone();
                    const next_type = try self.analyzeBox(fix.next, fix_hint, .Apply);
                    var next_iter = next_type.row_types.keyIterator();
                    while (next_iter.next()) |next_row_type| {
                        if (next_row_type.columns.len < 1)
                            return self.setError(expr_id, "The `next` argument for fix must have arity >= 1", .{}, .Other);
                        if (next_row_type.columns[0] != .Box)
                            return self.setError(expr_id, "The `next` argument for fix must take a box as it's first argument. Found {}", .{next_type}, .Other);
                        const new_fix_row_type = type_.RowType{ .columns = next_row_type.columns[1..] };
                        try new_fix_row_types.put(new_fix_row_type, {});
                    }
                    const new_fix_type = type_.SetType{ .row_types = new_fix_row_types };
                    if (u.deepEqual(fix_type, new_fix_type)) {
                        // reached fixpoint
                        // TODO check that fix_box_type doesn't escape
                        return fix_type;
                    }
                    fix_type = new_fix_type;
                    fix_box_type.FixOrReduce.set_type = new_fix_type;
                }
                return self.setError(expr_id, "Type of fix failed to converge, reached {}", .{fix_type}, .Other);
            },
            .Reduce => |reduce| {
                const input_type = try self.analyzeExpr(reduce.input, &.{}, .Apply);
                const input_box_type = type_.BoxType{ .FixOrReduce = .{
                    .def_id = reduce.next.def_id,
                    .set_type = input_type,
                } };
                const init_type = try self.analyzeExpr(reduce.init, &.{}, .Apply);
                var reduce_hint = try self.arena.allocator().alloc(type_.ScalarType, 2);
                var reduce_type = init_type;
                var reduce_box_type = type_.BoxType{
                    .FixOrReduce = .{
                        .def_id = reduce.next.def_id,
                        .set_type = init_type,
                    },
                };
                var max_iterations: usize = 10;
                while (max_iterations > 0) : (max_iterations -= 1) {
                    try self.interrupter.check();
                    reduce_hint[0] = .{ .Box = reduce_box_type };
                    reduce_hint[1] = .{ .Box = input_box_type };
                    var new_reduce_row_types = try reduce_type.row_types.clone();
                    const next_type = try self.analyzeBox(reduce.next, reduce_hint, .Apply);
                    var next_iter = next_type.row_types.keyIterator();
                    while (next_iter.next()) |next_row_type| {
                        if (next_row_type.columns.len < 2)
                            return self.setError(expr_id, "The `next` argument for reduce must have arity >= 1", .{}, .Other);

                        if (next_row_type.columns[0] != .Box or next_row_type.columns[1] != .Box)
                            return self.setError(expr_id, "The body for reduce must take boxes as it's first two arguments. Found {}", .{next_type}, .Other);
                        const new_reduce_row_type = type_.RowType{ .columns = next_row_type.columns[2..] };
                        try new_reduce_row_types.put(new_reduce_row_type, {});
                    }
                    const new_reduce_type = type_.SetType{ .row_types = new_reduce_row_types };
                    if (u.deepEqual(reduce_type, new_reduce_type)) {
                        // reached fixpoint
                        // TODO check that input_box_type and reduce_box_type do not escape
                        return reduce_type;
                    }
                    reduce_type = new_reduce_type;
                    reduce_box_type.FixOrReduce.set_type = new_reduce_type;
                }
                return self.setError(expr_id, "Type of reduce failed to converge, reached {}", .{reduce_type}, .Other);
            },
            .Enumerate => |body| {
                const body_type = try self.analyzeExpr(body, &.{}, .Apply);
                var row_types = u.DeepHashSet(type_.RowType).init(self.arena.allocator());
                var body_iter = body_type.row_types.keyIterator();
                while (body_iter.next()) |body_row_type| {
                    const columns = try std.mem.concat(self.arena.allocator(), type_.ScalarType, &.{
                        &[_]type_.ScalarType{.Number},
                        body_row_type.columns,
                    });
                    try row_types.put(.{ .columns = columns }, {});
                }
                return type_.SetType{ .row_types = row_types };
            },
            .Annotate => |annotate| {
                // TODO some annotations affect types eg solve
                return try self.analyzeExpr(annotate.body, hint, hint_mode);
            },
            .Watch => |watch| return try self.analyzeExpr(watch.body, hint, hint_mode),
            .Native => |native| {
                switch (native) {
                    .Add, .Subtract, .Multiply, .Divide, .Modulus, .Range, .GreaterThan, .GreaterThanOrEqual => {
                        if (hint.len < 2)
                            return switch (hint_mode) {
                                .Apply => self.setError(expr_id, "Could not infer the type of abstract arg", .{}, .NoHintForArg),
                                .Intersect => return type_.SetType.none(self.arena.allocator()),
                            };
                        if (hint[0] != .Number) {
                            try self.addWarning(expr_id, "First argument to {} should have type number, not {}", .{ native, hint[0] });
                            return type_.SetType.none(self.arena.allocator());
                        }
                        if (hint[1] != .Number) {
                            try self.addWarning(expr_id, "Second argument to {} should have type number, not {}", .{ native, hint[1] });
                            return type_.SetType.none(self.arena.allocator());
                        }
                        return type_.SetType.fromColumns(self.arena.allocator(), switch (native) {
                            .Add, .Subtract, .Multiply, .Divide, .Modulus, .Range => &.{ .Number, .Number, .Number },
                            .GreaterThan, .GreaterThanOrEqual => &.{ .Number, .Number },
                            else => unreachable,
                        });
                    },
                    .Number, .Text => {
                        if (hint.len < 1)
                            return switch (hint_mode) {
                                .Apply => self.setError(expr_id, "Could not infer the type of abstract arg", .{}, .NoHintForArg),
                                // TODO this is kind of a hack to support `as` - might break things if we ever use .Intersect in a place that can actually return `native`
                                .Intersect => type_.SetType.fromScalar(self.arena.allocator(), switch (native) {
                                    .Number => type_.ScalarType.Number,
                                    .Text => type_.ScalarType.Text,
                                    else => unreachable,
                                }),
                            };
                        const satisfied =
                            switch (native) {
                            .Number => hint[0] == .Number,
                            .Text => hint[0] == .Text,
                            else => unreachable,
                        };
                        return if (satisfied)
                            type_.SetType.fromScalar(self.arena.allocator(), hint[0])
                        else
                            type_.SetType.none(self.arena.allocator());
                    },
                }
            },
            .IsTest => |pair| {
                const left_type = try self.analyzeExpr(pair.left, &.{}, .Apply);
                var is_none = false;
                var left_iter = left_type.row_types.keyIterator();
                while (left_iter.next()) |left_row_type| {
                    const right_type = try self.analyzeExpr(pair.right, left_row_type.columns, .Intersect);
                    if (!right_type.row_types.contains(left_row_type.*)) is_none = true;
                }
                return if (is_none)
                    type_.SetType.none(self.arena.allocator())
                else
                    type_.SetType.some(self.arena.allocator());
            },
            .IsAssert => |pair| {
                const left_type = try self.analyzeExpr(pair.left, &.{}, .Apply);
                var left_iter = left_type.row_types.keyIterator();
                var row_types = u.DeepHashSet(type_.RowType).init(self.arena.allocator());
                while (left_iter.next()) |left_row_type| {
                    const right_type = try self.analyzeExpr(pair.right, left_row_type.columns, .Intersect);
                    if (right_type.row_types.contains(left_row_type.*))
                        _ = try row_types.put(left_row_type.*, {})
                    else
                        try self.addWarning(expr_id, "This `is!` might fail: {} is not contained in {}", .{ left_row_type, right_type });
                }
                return type_.SetType{ .row_types = row_types };
            },
            .As => |pair| {
                _ = try self.analyzeExpr(pair.left, &.{}, .Apply);
                // TODO using .Intersect here is a total hack - will totally break if we use a non-type thing on the right eg `?a a number`
                const right_type = try self.analyzeExpr(pair.right, &.{}, .Intersect);
                return right_type;
            },
        }
    }

    fn analyzeIntersect(self: *Analyzer, intersect_expr_id: core.ExprId, left_type: type_.SetType, right_expr_id: core.ExprId, is_flipped: bool) Error!type_.SetType {
        var row_types = u.DeepHashSet(type_.RowType).init(self.arena.allocator());
        var right_row_types = u.DeepHashSet(type_.RowType).init(self.arena.allocator());
        var left_iter = left_type.row_types.keyIterator();
        while (left_iter.next()) |left_row_type| {
            const right_type = try self.analyzeExpr(right_expr_id, left_row_type.columns, .Intersect);
            var right_iter = right_type.row_types.keyIterator();
            while (right_iter.next()) |right_row_type| {
                try right_row_types.put(right_row_type.*, {});
            }
            if (right_type.row_types.contains(left_row_type.*)) {
                try row_types.put(left_row_type.*, {});
            }
        }
        const total_right_type = type_.SetType{ .row_types = right_row_types };
        if (row_types.count() == 0 and right_row_types.count() != 0)
            if (is_flipped)
                try self.addWarning(intersect_expr_id, "The intersection of {} with {} will always be empty", .{ left_type, total_right_type })
            else
                try self.addWarning(intersect_expr_id, "The intersection of {} with {} will always be empty", .{ total_right_type, left_type });
        return type_.SetType{ .row_types = row_types };
    }

    fn analyzeApplyFinite(self: *Analyzer, apply_expr_id: core.ExprId, left_type: type_.SetType, right_type: type_.SetType) Error!type_.SetType {
        var row_types = u.DeepHashSet(type_.RowType).init(self.arena.allocator());
        var left_iter = left_type.row_types.keyIterator();
        while (left_iter.next()) |left_row_type| {
            var right_iter = right_type.row_types.keyIterator();
            while (right_iter.next()) |right_row_type| {
                const joined_arity = u.min(left_row_type.columns.len, right_row_type.columns.len);

                var has_intersection = true;
                for (left_row_type.columns[0..joined_arity]) |left_column, i| {
                    if (!u.deepEqual(left_column, right_row_type.columns[i]))
                        has_intersection = false;
                }

                if (has_intersection) {
                    const columns = if (left_row_type.columns.len > right_row_type.columns.len)
                        left_row_type.columns[joined_arity..]
                    else
                        right_row_type.columns[joined_arity..];
                    try row_types.put(.{ .columns = columns }, {});
                }
            }
        }
        if (row_types.count() == 0 and
            left_type.row_types.count() != 0 and
            right_type.row_types.count() != 0 and
            self.core_program.exprs[apply_expr_id.id].Apply.kind == .User)
            try self.addWarning(apply_expr_id, "The result of applying {} to {} will always be empty", .{ left_type, right_type });
        return type_.SetType{ .row_types = row_types };
    }

    fn analyzeApplyNonfinite(self: *Analyzer, apply_expr_id: core.ExprId, left_type: type_.SetType, right_expr_id: core.ExprId, hint: []const type_.ScalarType, hint_mode: type_.HintMode, is_flipped: bool) Error!type_.SetType {
        var row_types = u.DeepHashSet(type_.RowType).init(self.arena.allocator());
        var left_iter = left_type.row_types.keyIterator();
        while (left_iter.next()) |left_row_type| {
            var right_hint = try u.ArrayList(type_.ScalarType).initCapacity(self.arena.allocator(), left_row_type.columns.len + hint.len);
            for (left_row_type.columns) |column_type|
                try right_hint.append(column_type);
            try right_hint.appendSlice(hint);
            const right_type = try self.analyzeExpr(right_expr_id, right_hint.toOwnedSlice(), hint_mode);
            var left_has_intersection = false;
            var right_iter = right_type.row_types.keyIterator();
            while (right_iter.next()) |right_row_type| {
                const joined_arity = u.min(left_row_type.columns.len, right_row_type.columns.len);

                var has_intersection = true;
                for (left_row_type.columns[0..joined_arity]) |left_column, i| {
                    if (!u.deepEqual(left_column, right_row_type.columns[i]))
                        has_intersection = false;
                }

                if (has_intersection) {
                    left_has_intersection = true;
                    const columns = if (left_row_type.columns.len > right_row_type.columns.len)
                        left_row_type.columns[joined_arity..]
                    else
                        right_row_type.columns[joined_arity..];
                    try row_types.put(.{ .columns = columns }, {});
                }
            }
            if (!left_has_intersection and
                self.core_program.exprs[apply_expr_id.id].Apply.kind == .User)
            {
                if (is_flipped)
                    try self.addWarning(apply_expr_id, "In apply: right row type {} has no match in left set type {}", .{ left_row_type, right_type })
                else
                    try self.addWarning(apply_expr_id, "In apply: left row type {} has no match in right set type {}", .{ left_row_type, right_type });
            }
        }
        return type_.SetType{ .row_types = row_types };
    }
};
