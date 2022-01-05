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

    fn analyzeDef(self: *Analyzer, def_id: core.DefId, hint: []const type_.ScalarType) Error!type_.SetType {
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
        var used_hint_ix: ?usize = null;
        var iter = set_type.concretes.keyIterator();
        while (iter.next()) |concrete| {
            used_hint_ix = u.max(used_hint_ix orelse 0, concrete.columns.len);
        }
        const used_hint = hint[0..u.min(hint.len, used_hint_ix orelse 0)];
        try self.defs[def_id.id].append(.{
            .hint = try self.arena.allocator().dupe(type_.ScalarType, used_hint),
            .set_type = set_type,
        });

        return set_type;
    }

    fn analyzeBox(self: *Analyzer, box: core.Box, hint: []const type_.ScalarType) Error!type_.SetType {
        var box_hint = u.ArrayList(type_.ScalarType).init(self.arena.allocator());
        for (box.args) |scalar_id|
            try box_hint.append(self.scope.items[self.scope.items.len - 1 - scalar_id.id]);
        try box_hint.appendSlice(hint);
        const set_type = try self.analyzeDef(box.def_id, box_hint.items);
        var concretes = u.DeepHashSet(type_.ConcreteSetType).init(self.arena.allocator());
        var concretes_iter = set_type.concretes.keyIterator();
        while (concretes_iter.next()) |concrete| {
            try concretes.put(.{ .columns = concrete.columns[box.args.len..] }, {});
        }
        return type_.SetType{ .concretes = concretes };
    }

    fn analyzeExpr(self: *Analyzer, expr_id: core.ExprId, hint: []const type_.ScalarType) Error!type_.SetType {
        const set_type = try self.analyzeExprInner(expr_id, hint);
        return set_type;
    }

    fn analyzeExprInner(self: *Analyzer, expr_id: core.ExprId, hint: []const type_.ScalarType) Error!type_.SetType {
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
                    .StagedText => |text| .{ .StagedText = text },
                    .StagedNumber => |number| .{ .StagedNumber = number },
                };
                return type_.SetType.fromScalar(self.arena.allocator(), scalar_type);
            },
            .Union, .Intersect => |pair| {
                var concretes = u.DeepHashSet(type_.ConcreteSetType).init(self.arena.allocator());
                const left = try self.analyzeExpr(pair.left, hint);
                const right = try self.analyzeExpr(pair.right, hint);
                switch (expr) {
                    .Union => {
                        var left_iter = left.concretes.keyIterator();
                        while (left_iter.next()) |left_concrete| {
                            try concretes.put(left_concrete.*, {});
                        }
                        var right_iter = right.concretes.keyIterator();
                        while (right_iter.next()) |right_concrete| {
                            try concretes.put(right_concrete.*, {});
                        }
                    },
                    .Intersect => {
                        var left_iter = left.concretes.keyIterator();
                        while (left_iter.next()) |left_concrete| {
                            if (right.concretes.contains(left_concrete.*)) {
                                try concretes.put(left_concrete.*, {});
                            }
                        }
                        if (concretes.count() == 0)
                            return self.setError(expr_id, "Intersection of {} and {} will always be empty", .{ left, right }, .Other);
                    },
                    else => unreachable,
                }
                return self.checkSetType(expr_id, .{ .concretes = concretes });
            },
            .Product => |pair| {
                var concretes = u.DeepHashSet(type_.ConcreteSetType).init(self.arena.allocator());
                const left = try self.analyzeExpr(pair.left, hint);
                var left_iter = left.concretes.keyIterator();
                while (left_iter.next()) |left_concrete| {
                    const right = try self.analyzeExpr(pair.right, hint[u.min(hint.len, left_concrete.columns.len)..]);
                    var right_iter = right.concretes.keyIterator();
                    while (right_iter.next()) |right_concrete| {
                        const columns_inputs = [2][]const type_.ColumnType{
                            left_concrete.columns,
                            right_concrete.columns,
                        };
                        const columns = try std.mem.concat(self.arena.allocator(), type_.ColumnType, &columns_inputs);
                        try concretes.put(.{ .columns = columns }, {});
                    }
                }
                return type_.SetType{ .concretes = concretes };
            },
            .Equal => |pair| {
                // the hint for expr doesn't tell us anything about left or right
                const left = try self.analyzeExpr(pair.left, &.{});
                const right = try self.analyzeExpr(pair.right, &.{});
                if (!left.isFinite() or !right.isFinite()) {
                    return self.setError(expr_id, "Cannot equal one or more maybe-infinite sets: {} = {}", .{ left, right }, .Other);
                }
                var is_intersection_empty = true;
                var left_iter = left.concretes.keyIterator();
                while (left_iter.next()) |left_concrete| {
                    if (right.concretes.contains(left_concrete.*)) {
                        is_intersection_empty = false;
                    }
                }
                if (is_intersection_empty)
                    return self.setError(expr_id, "Will never be equal: {} vs {}", .{ left, right }, .Other);
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
                                const set_type = try self.analyzeDef(normal.def_id, box_hint);
                                var concretes = u.DeepHashSet(type_.ConcreteSetType).init(self.arena.allocator());
                                var concretes_iter = set_type.concretes.keyIterator();
                                while (concretes_iter.next()) |concrete| {
                                    try concretes.put(.{ .columns = concrete.columns[normal.args.len..] }, {});
                                }
                                return type_.SetType{ .concretes = concretes };
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
                return try type_.SetType.some(self.arena.allocator());
            },
            .Then => |then| {
                // the hint for expr doesn't tell us anything about condition
                const condition_type = try self.analyzeExpr(then.condition, &.{});
                if (!condition_type.isBoolish()) {
                    return self.setError(expr_id, "The condition of `then` must have type `maybe`, found {}", .{condition_type}, .Other);
                }
                return if (condition_type.concretes.count() == 0)
                    type_.SetType.none(self.arena.allocator())
                else
                    try self.analyzeExpr(then.true_branch, hint);
            },
            .Abstract => |body| {
                if (hint.len == 0) {
                    return self.setError(expr_id, "Could not infer the type of abstract arg", .{}, .NoHintForArg);
                } else {
                    // if we have a hint we can use it to specialize the body
                    try self.scope.append(hint[0]);
                    defer _ = self.scope.pop();
                    const body_type = try self.analyzeExpr(body, hint[1..]);
                    var concretes = u.DeepHashSet(type_.ConcreteSetType).init(self.arena.allocator());
                    var body_iter = body_type.concretes.keyIterator();
                    while (body_iter.next()) |body_concrete| {
                        const columns = try std.mem.concat(self.arena.allocator(), type_.ColumnType, &.{
                            &[_]type_.ColumnType{.{ .abstract = true, .value = hint[0] }},
                            body_concrete.columns,
                        });
                        try concretes.put(.{ .columns = columns }, {});
                    }
                    return type_.SetType{ .concretes = concretes };
                }
            },
            .Apply => |pair| {
                // can't make use of hint until we know which side is finite
                if (self.analyzeExpr(pair.left, &.{})) |left_type| {
                    if (left_type.isFinite())
                        return self.analyzeApply(expr_id, left_type, pair.right, hint, false);
                } else |left_err| {
                    if (self.error_info.*.?.kind != .NoHintForArg)
                        return left_err;
                    self.error_info.* = null;
                }

                // error was from lack of hints, so try other way around
                // TODO could this cause exponential retries in large program?
                if (self.analyzeExpr(pair.right, &.{})) |right_type| {
                    return self.analyzeApply(expr_id, right_type, pair.left, hint, true);
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
                var max_iterations: usize = 10;
                while (max_iterations > 0) : (max_iterations -= 1) {
                    try self.interrupter.check();
                    fix_hint[0] = .{ .Box = fix_box_type };
                    var new_fix_concretes = try fix_type.concretes.clone();
                    const next_type = try self.analyzeBox(fix.next, fix_hint);
                    var next_iter = next_type.concretes.keyIterator();
                    while (next_iter.next()) |next_concrete| {
                        if (next_concrete.columns.len < 1)
                            return self.setError(expr_id, "The `next` argument for fix must have arity >= 1", .{}, .Other);
                        if (next_concrete.columns[0].value != .Box)
                            return self.setError(expr_id, "The `next` argument for fix must take a box as it's first argument. Found {}", .{next_type}, .Other);
                        const new_fix_concrete = type_.ConcreteSetType{ .columns = next_concrete.columns[1..] };
                        if (!new_fix_concrete.isFinite())
                            return self.setError(expr_id, "The body for fix must have finite type, found {}", .{next_type}, .Other);
                        try new_fix_concretes.put(new_fix_concrete, {});
                    }
                    const new_fix_type = type_.SetType{ .concretes = new_fix_concretes };
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
                var max_iterations: usize = 10;
                while (max_iterations > 0) : (max_iterations -= 1) {
                    try self.interrupter.check();
                    reduce_hint[0] = .{ .Box = reduce_box_type };
                    reduce_hint[1] = .{ .Box = input_box_type };
                    var new_reduce_concretes = try reduce_type.concretes.clone();
                    const next_type = try self.analyzeBox(reduce.next, reduce_hint);
                    var next_iter = next_type.concretes.keyIterator();
                    while (next_iter.next()) |next_concrete| {
                        if (next_concrete.columns.len < 2)
                            return self.setError(expr_id, "The `next` argument for reduce must have arity >= 1", .{}, .Other);

                        if (next_concrete.columns[0].value != .Box or next_concrete.columns[1].value != .Box)
                            return self.setError(expr_id, "The body for reduce must take boxes as it's first two arguments. Found {}", .{next_type}, .Other);
                        const new_reduce_concrete = type_.ConcreteSetType{ .columns = next_concrete.columns[2..] };
                        if (!new_reduce_concrete.isFinite()) {
                            return self.setError(expr_id, "The body for reduce must have finite type, found {}", .{next_type}, .Other);
                        }
                        try new_reduce_concretes.put(new_reduce_concrete, {});
                    }
                    const new_reduce_type = type_.SetType{ .concretes = new_reduce_concretes };
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
                const body_type = try self.analyzeExpr(body, &.{});
                if (!body_type.isFinite()) {
                    return self.setError(expr_id, "The body of `enumerate` must have finite type, found {}", .{body_type}, .Other);
                }
                var concretes = u.DeepHashSet(type_.ConcreteSetType).init(self.arena.allocator());
                var body_iter = body_type.concretes.keyIterator();
                while (body_iter.next()) |body_concrete| {
                    const columns = try std.mem.concat(self.arena.allocator(), type_.ColumnType, &.{
                        &[_]type_.ColumnType{.{ .abstract = false, .value = .Number }},
                        body_concrete.columns,
                    });
                    try concretes.put(.{ .columns = columns }, {});
                }
                return type_.SetType{ .concretes = concretes };
            },
            .Annotate => |annotate| {
                // TODO some annotations affect types eg solve
                return try self.analyzeExpr(annotate.body, hint);
            },
            .Watch => |watch| return try self.analyzeExpr(watch.body, hint),
            .Native => |native| {
                return type_.SetType.fromColumns(self.arena.allocator(), switch (native) {
                    .Add, .Subtract, .Multiply, .Divide, .Modulus, .Range => &.{
                        .{ .abstract = true, .value = .Number },
                        .{ .abstract = true, .value = .Number },
                        .{ .abstract = false, .value = .Number },
                    },
                    .GreaterThan, .GreaterThanOrEqual => &.{
                        .{ .abstract = true, .value = .Number },
                        .{ .abstract = true, .value = .Number },
                    },
                });
            },
        }
    }

    fn analyzeApply(self: *Analyzer, apply_expr_id: core.ExprId, left_type: type_.SetType, right_expr_id: core.ExprId, hint: []const type_.ScalarType, is_flipped: bool) Error!type_.SetType {
        var concretes = u.DeepHashSet(type_.ConcreteSetType).init(self.arena.allocator());
        var left_iter = left_type.concretes.keyIterator();
        while (left_iter.next()) |left_concrete| {
            var right_hint = try u.ArrayList(type_.ScalarType).initCapacity(self.arena.allocator(), left_concrete.columns.len + hint.len);
            for (left_concrete.columns) |column_type|
                try right_hint.append(column_type.value);
            try right_hint.appendSlice(hint);
            const right_type = try self.analyzeExpr(right_expr_id, right_hint.toOwnedSlice());
            var right_iter = right_type.concretes.keyIterator();
            while (right_iter.next()) |right_concrete| {
                const joined_arity = u.min(left_concrete.columns.len, right_concrete.columns.len);

                var has_intersection = true;
                for (left_concrete.columns[0..joined_arity]) |left_column, i| {
                    if (!u.deepEqual(left_column.value, right_concrete.columns[i].value))
                        has_intersection = false;
                }

                if (has_intersection) {
                    const columns = if (left_concrete.columns.len > right_concrete.columns.len)
                        left_concrete.columns[joined_arity..]
                    else
                        right_concrete.columns[joined_arity..];
                    try concretes.put(.{ .columns = columns }, {});
                } else {
                    if (analyzeConcreteSetTypeUnion(left_concrete.*, right_concrete.*) == .Conflicting)
                        return if (is_flipped)
                            self.setError(apply_expr_id, "Can't apply {} to {}", .{ right_concrete, left_concrete }, .Other)
                        else
                            self.setError(apply_expr_id, "Can't apply {} to {}", .{ left_concrete, right_concrete }, .Other);
                }
            }
        }
        return self.checkSetType(apply_expr_id, .{ .concretes = concretes });
    }

    fn checkSetType(self: *Analyzer, expr_id: core.ExprId, set_type: type_.SetType) Error!type_.SetType {
        var iter1 = set_type.concretes.keyIterator();
        while (iter1.next()) |concrete1| {
            var iter2 = set_type.concretes.keyIterator();
            while (iter2.next()) |concrete2| {
                switch (analyzeConcreteSetTypeUnion(concrete1.*, concrete2.*)) {
                    .Ok => {},
                    .Conflicting => return self.setError(expr_id, "Cannot union types {} and {}", .{ concrete1, concrete2 }, .Other),
                }
            }
        }
        return set_type;
    }

    /// For every two concrete types in a SetType, the first place at which they differ must be a staged type in both
    fn analyzeConcreteSetTypeUnion(concrete1: type_.ConcreteSetType, concrete2: type_.ConcreteSetType) enum { Ok, Conflicting } {
        const min_len = u.min(concrete1.columns.len, concrete2.columns.len);
        var i: usize = 0;
        while (i < min_len) : (i += 1)
            if (!u.deepEqual(concrete1.columns[i], concrete2.columns[i]))
                return if (concrete1.columns[i].value.isStaged() and concrete2.columns[i].value.isStaged())
                    .Ok
                else
                    .Conflicting;
        return if (concrete1.columns.len == concrete2.columns.len)
            .Ok
        else
            .Conflicting;
    }
};
