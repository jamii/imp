const imp = @import("../../../imp.zig");
usingnamespace imp.common;
const meta = imp.meta;
const Store = imp.lang.Store;
const syntax = imp.lang.repr.syntax;
const core = imp.lang.repr.core2;
const type_ = imp.lang.repr.type_2;
const value = imp.lang.repr.value2;

pub fn interpret(
    store: *const Store,
    arena: *ArenaAllocator,
    program: core.Program,
    program_type: type_.ProgramType,
    watch_results: *DeepHashSet(WatchResult),
    interrupter: imp.lang.Interrupter,
    error_info: *?ErrorInfo,
) Error!value.Set {
    var def_sets = try arena.allocator.alloc(ArrayList(Interpreter.Memo), program.def_exprs.len);
    for (def_sets) |*def_set| def_set.* = ArrayList(Interpreter.Memo).init(&arena.allocator);
    var interpreter = Interpreter{
        .store = store,
        .arena = arena,
        .program = program,
        .program_type = program_type,
        .watch_results = watch_results,
        .def_sets = def_sets,
        .scope = ArrayList(value.Scalar).init(&store.arena.allocator),
        .time = ArrayList(usize).init(&store.arena.allocator),
        .interrupter = interrupter,
        .error_info = error_info,
    };
    return interpreter.interpretDef(program.def_exprs.len - 1, &.{});
}

pub const Error = error{
    // sets error_info
    Interpret2Error,
    Native2Error,

    // does not set error_info
    OutOfMemory,
    WasInterrupted,
};

pub const ErrorInfo = struct {
    expr: *const core.Expr,
    message: []const u8,
};

pub const WatchResult = struct {
    time: []const usize,
    scope: []const ScopeItem,
    set: value.Set,

    pub const ScopeItem = struct {
        name: syntax.Name,
        scalar: value.Scalar,
    };
};

// --------------------------------------------------------------------------------

const Interpreter = struct {
    store: *const Store,
    arena: *ArenaAllocator,
    program: core.Program,
    program_type: type_.ProgramType,
    watch_results: *DeepHashSet(WatchResult),
    def_sets: []ArrayList(Memo),
    scope: ArrayList(value.Scalar),
    time: ArrayList(usize),
    interrupter: imp.lang.Interrupter,
    error_info: *?ErrorInfo,

    pub const Memo = struct {
        hint: value.Tuple,
        set: value.Set,
    };

    fn setError(self: *Interpreter, expr: *const core.Expr, comptime fmt: []const u8, args: anytype) Error {
        const message = try format(&self.arena.allocator, fmt, args);
        self.error_info.* = ErrorInfo{
            .expr = expr,
            .message = message,
        };
        return error.Interpret2Error;
    }

    fn setNativeError(self: *Interpreter, expr: *const core.Expr, comptime fmt: []const u8, args: anytype) Error {
        const message = try format(&self.arena.allocator, fmt, args);
        self.error_info.* = ErrorInfo{
            .expr = expr,
            .message = message,
        };
        return error.Native2Error;
    }

    fn interpretDef(self: *Interpreter, def_id: core.DefId, hint: value.Tuple) Error!value.Set {
        const def_set = &self.def_sets[def_id];

        // check if already evaluated
        // TODO it's surprisingly hard to write a better lookup than this
        for (def_set.items) |memo|
            if (meta.deepEqual(memo.hint, hint))
                return memo.set;

        // otherwise, evaluate
        const old_scope = self.scope;
        defer self.scope = old_scope;
        self.scope = ArrayList(value.Scalar).init(&self.store.arena.allocator);
        const old_time = self.time;
        defer self.time = old_time;
        self.time = ArrayList(usize).init(&self.store.arena.allocator);
        const set = try self.interpretExpr(self.program.def_exprs[def_id], hint);

        // memoize
        try def_set.append(.{ .hint = hint, .set = set });

        return set;
    }

    fn interpretBox(self: *Interpreter, box: core.Box, hint: value.Tuple) Error!value.Set {
        const box_args = try self.arena.allocator.alloc(value.Scalar, box.args.len);
        for (box_args) |*box_arg, i|
            box_arg.* = self.scope.items[self.scope.items.len - 1 - box.args[i]];
        const box_hint = try std.mem.concat(&self.arena.allocator, value.Scalar, &.{ box_args, hint });
        const set = try self.interpretDef(box.def_id, box_hint);
        var result_set = value.Set{
            .arity = set.arity - box.args.len,
            .set = DeepHashSet(value.Tuple).init(&self.arena.allocator),
        };
        var set_iter = set.set.iterator();
        while (set_iter.next()) |entry|
            if (meta.deepEqual(entry.key_ptr.*[0..box.args.len], box_args))
                try result_set.set.put(entry.key_ptr.*[box.args.len..], {});
        return result_set;
    }

    fn interpretExpr(self: *Interpreter, expr: *const core.Expr, hint: value.Tuple) Error!value.Set {
        try self.interrupter.check();
        switch (expr.*) {
            .None => {
                const set = DeepHashSet(value.Tuple).init(&self.arena.allocator);
                return value.Set{
                    .arity = 0,
                    .set = set,
                };
            },
            .Some => {
                var set = DeepHashSet(value.Tuple).init(&self.arena.allocator);
                _ = try set.put(&.{}, {});
                return value.Set{
                    .arity = 0,
                    .set = set,
                };
            },
            .Scalar => |scalar| {
                var set = DeepHashSet(value.Tuple).init(&self.arena.allocator);
                _ = try set.put(try self.dupeScalars(&.{scalar}), {});
                return value.Set{
                    .arity = 1,
                    .set = set,
                };
            },
            .Union => |pair| {
                const left = try self.interpretExpr(pair.left, hint);
                const right = try self.interpretExpr(pair.right, hint);
                if (left.arity != right.arity and left.set.count() > 0 and right.set.count() > 0) {
                    return self.setError(expr, "Tried to union sets with different arities: {} vs {}", .{ left.arity, right.arity });
                }
                var set = DeepHashSet(value.Tuple).init(&self.arena.allocator);
                var left_iter = left.set.iterator();
                while (left_iter.next()) |kv| {
                    try self.interrupter.check();
                    _ = try set.put(kv.key_ptr.*, {});
                }
                var right_iter = right.set.iterator();
                while (right_iter.next()) |kv| {
                    try self.interrupter.check();
                    _ = try set.put(kv.key_ptr.*, {});
                }
                return value.Set{
                    .arity = max(left.arity, right.arity),
                    .set = set,
                };
            },
            .Intersect => |pair| {
                const left = try self.interpretExpr(pair.left, hint);
                const right = try self.interpretExpr(pair.right, hint);
                if (left.arity != right.arity and left.set.count() > 0 and right.set.count() > 0) {
                    return self.setError(expr, "Tried to intersect sets with different arities: {} vs {}", .{ left.arity, right.arity });
                }
                var set = DeepHashSet(value.Tuple).init(&self.arena.allocator);
                var left_iter = left.set.iterator();
                while (left_iter.next()) |kv| {
                    try self.interrupter.check();
                    if (right.set.contains(kv.key_ptr.*)) {
                        _ = try set.put(kv.key_ptr.*, {});
                    }
                }
                return value.Set{
                    .arity = max(left.arity, right.arity),
                    .set = set,
                };
            },
            .Product => |pair| {
                const left = try self.interpretExpr(pair.left, hint);
                const right = try self.interpretExpr(pair.right, hint[min(hint.len, left.arity)..]);
                var set = DeepHashSet(value.Tuple).init(&self.arena.allocator);
                var left_iter = left.set.iterator();
                while (left_iter.next()) |lkv| {
                    try self.interrupter.check();
                    var right_iter = right.set.iterator();
                    while (right_iter.next()) |rkv| {
                        try self.interrupter.check();
                        var tuple = try self.arena.allocator.alloc(value.Scalar, lkv.key_ptr.len + rkv.key_ptr.len);
                        var i: usize = 0;
                        for (lkv.key_ptr.*) |scalar| {
                            tuple[i] = scalar;
                            i += 1;
                        }
                        for (rkv.key_ptr.*) |scalar| {
                            tuple[i] = scalar;
                            i += 1;
                        }
                        _ = try set.put(tuple, {});
                    }
                }
                return value.Set{
                    .arity = left.arity + right.arity,
                    .set = set,
                };
            },
            .Equal => |pair| {
                // the hint for expr doesn't tell us anything about left or right
                const left = try self.interpretExpr(pair.left, &.{});
                const right = try self.interpretExpr(pair.right, &.{});
                var set = DeepHashSet(value.Tuple).init(&self.arena.allocator);
                const isEqual = isEqual: {
                    var leftIter = left.set.iterator();
                    while (leftIter.next()) |kv| {
                        try self.interrupter.check();
                        if (!right.set.contains(kv.key_ptr.*)) {
                            break :isEqual false;
                        }
                    }
                    var rightIter = right.set.iterator();
                    while (rightIter.next()) |kv| {
                        try self.interrupter.check();
                        if (!left.set.contains(kv.key_ptr.*)) {
                            break :isEqual false;
                        }
                    }
                    break :isEqual true;
                };
                if (isEqual) {
                    _ = try set.put(&.{}, {});
                }
                return value.Set{
                    .arity = 0,
                    .set = set,
                };
            },
            .ScalarId => |scalar_id| {
                var set = DeepHashSet(value.Tuple).init(&self.arena.allocator);
                const scalar = self.scope.items[self.scope.items.len - 1 - scalar_id];
                _ = try set.put(try self.dupeScalars(&.{scalar}), {});
                return value.Set{
                    .arity = 1,
                    .set = set,
                };
            },
            .UnboxScalarId => |scalar_id| {
                const scalar = self.scope.items[self.scope.items.len - 1 - scalar_id];
                switch (scalar) {
                    .Box => |box| {
                        switch (box) {
                            .Normal => |normal| {
                                const box_hint = try std.mem.concat(
                                    &self.store.arena.allocator,
                                    value.Scalar,
                                    &.{
                                        normal.args,
                                        hint,
                                    },
                                );
                                const set = try self.interpretDef(normal.def_id, box_hint);
                                var result_set = value.Set{
                                    .arity = set.arity - normal.args.len,
                                    .set = DeepHashSet(value.Tuple).init(&self.arena.allocator),
                                };
                                var set_iter = set.set.iterator();
                                while (set_iter.next()) |entry|
                                    if (meta.deepEqual(entry.key_ptr.*[0..normal.args.len], normal.args))
                                        try result_set.set.put(entry.key_ptr.*[normal.args.len..], {});
                                return result_set;
                            },
                            .FixOrReduce => |_| return box.getFixOrReduce(),
                        }
                    },
                    else => {
                        return self.setError(expr, "Tried to unbox {} which is not a box", .{scalar});
                    },
                }
            },
            .DefId => |def_id| return self.interpretDef(def_id, hint),
            .Negate => |body| {
                // the hint for expr doesn't tell us anything about body
                const body_set = try self.interpretExpr(body, &.{});
                var set = DeepHashSet(value.Tuple).init(&self.arena.allocator);
                if (body_set.set.count() == 0) {
                    _ = try set.put(&.{}, {});
                }
                return value.Set{
                    .arity = 0,
                    .set = set,
                };
            },
            .Then => |then| {
                // the hint for expr doesn't tell us anything about condition
                const condition = try self.interpretExpr(then.condition, &.{});
                if (condition.set.count() == 0) {
                    const set = DeepHashSet(value.Tuple).init(&self.arena.allocator);
                    return value.Set{
                        .arity = 0,
                        .set = set,
                    };
                } else {
                    return self.interpretExpr(then.true_branch, hint);
                }
            },
            .Abstract => |body| {
                if (hint.len == 0) {
                    return self.setError(expr, "No hint for arg", .{});
                } else {
                    try self.scope.append(hint[0]);
                    const body_set = try self.interpretExpr(body, hint[1..]);
                    _ = self.scope.pop();
                    const arity = body_set.arity + 1;
                    var set = DeepHashSet(value.Tuple).init(&self.arena.allocator);
                    var body_set_iter = body_set.set.iterator();
                    while (body_set_iter.next()) |kv| {
                        try self.interrupter.check();
                        var tuple = try ArrayList(value.Scalar).initCapacity(&self.arena.allocator, kv.key_ptr.len + 1);
                        try tuple.append(hint[0]);
                        try tuple.appendSlice(kv.key_ptr.*);
                        _ = try set.put(tuple.items, {});
                    }
                    return value.Set{
                        .arity = arity,
                        .set = set,
                    };
                }
            },
            .Apply => |pair| {
                // can't make use of hint until we know which side is finite
                if (self.interpretExpr(pair.left, &.{})) |left_type| {
                    return self.interpretApply(expr, left_type, pair.right, hint);
                } else |_| {
                    // error might have been from lack of hints, so try other way around
                    // TODO could this cause exponential retries in large program?
                    if (self.interpretExpr(pair.right, &.{})) |right_type| {
                        return self.interpretApply(expr, right_type, pair.left, hint);
                    } else |err| {
                        return err;
                    }
                }
            },
            .Box => |box| {
                var args = try self.arena.allocator.alloc(value.Scalar, box.args.len);
                for (box.args) |name_ix, i| {
                    args[i] = self.scope.items[self.scope.items.len - 1 - name_ix];
                }
                const scalar = value.Box{
                    .Normal = .{
                        .def_id = box.def_id,
                        .args = args,
                    },
                };
                const tuple = try self.dupeScalars(&[1]value.Scalar{.{ .Box = scalar }});
                var set = DeepHashSet(value.Tuple).init(&self.arena.allocator);
                _ = try set.put(tuple, {});
                return value.Set{
                    .arity = 1,
                    .set = set,
                };
            },
            .Fix => |fix| {
                try self.time.append(0);
                const init_set = try self.interpretExpr(fix.init, &.{});
                _ = self.time.pop();
                const init_box = try value.Box.fixOrReduce(&self.arena.allocator, init_set);

                var fix_hint = try self.arena.allocator.alloc(value.Scalar, 1);

                var fix_box = init_box;
                var fix_set = init_set;
                var iteration: usize = 1;
                while (true) : (iteration += 1) {
                    try self.interrupter.check();
                    try self.time.append(iteration);
                    defer _ = self.time.pop();
                    fix_hint[0] = .{ .Box = fix_box };
                    const body_set = try self.interpretBox(fix.next, fix_hint);
                    var new_fix_set = value.Set{
                        .arity = if (body_set.arity == 0) 0 else body_set.arity - 1,
                        .set = DeepHashSet(value.Tuple).init(&self.arena.allocator),
                    };
                    var body_iter = body_set.set.iterator();
                    while (body_iter.next()) |kv| {
                        try self.interrupter.check();
                        if (meta.deepEqual(kv.key_ptr.*[0], value.Scalar{ .Box = fix_box })) {
                            _ = try new_fix_set.set.put(kv.key_ptr.*[1..], {});
                        }
                    }
                    if (meta.deepEqual(fix_set, new_fix_set)) {
                        return fix_set;
                    }
                    fix_set = new_fix_set;
                    fix_box = try value.Box.fixOrReduce(&self.arena.allocator, fix_set);
                }
            },
            .Reduce => |reduce| {
                const input_set = try self.interpretExpr(reduce.input, &.{});
                var input_tuples = try ArrayList(value.Tuple).initCapacity(&self.arena.allocator, input_set.set.count());
                var input_iter = input_set.set.iterator();
                while (input_iter.next()) |kv| {
                    try self.interrupter.check();
                    try input_tuples.append(kv.key_ptr.*);
                }
                // TODO would like to be able to interrupt sorting
                std.sort.sort(value.Tuple, input_tuples.items, {}, struct {
                    fn lessThan(_: void, a: value.Tuple, b: value.Tuple) bool {
                        return meta.deepCompare(a, b) == .LessThan;
                    }
                }.lessThan);

                try self.time.append(0);
                const init_set = try self.interpretExpr(reduce.init, &.{});
                _ = self.time.pop();
                const init_box = try value.Box.fixOrReduce(&self.arena.allocator, init_set);

                var reduce_hint = try self.arena.allocator.alloc(value.Scalar, 2);

                var reduce_box = init_box;
                var reduce_set = init_set;
                for (input_tuples.items) |input_tuple, iteration| {
                    try self.interrupter.check();
                    try self.time.append(iteration);
                    defer _ = self.time.pop();
                    var tuple_set = value.Set{
                        .arity = input_tuple.len,
                        .set = DeepHashSet(value.Tuple).init(&self.arena.allocator),
                    };
                    _ = try tuple_set.set.put(input_tuple, {});
                    const tuple_box = try value.Box.fixOrReduce(&self.arena.allocator, tuple_set);
                    reduce_hint[0] = .{ .Box = reduce_box };
                    reduce_hint[1] = .{ .Box = tuple_box };
                    const body_set = try self.interpretBox(reduce.next, reduce_hint);
                    var new_reduce_set = value.Set{
                        .arity = if (body_set.arity == 0) 0 else body_set.arity - 2,
                        .set = DeepHashSet(value.Tuple).init(&self.arena.allocator),
                    };
                    var body_iter = body_set.set.iterator();
                    while (body_iter.next()) |kv| {
                        try self.interrupter.check();
                        if (meta.deepEqual(kv.key_ptr.*[0], value.Scalar{ .Box = reduce_box }) and meta.deepEqual(kv.key_ptr.*[1], value.Scalar{ .Box = tuple_box })) {
                            _ = try new_reduce_set.set.put(kv.key_ptr.*[2..], {});
                        }
                    }
                    reduce_set = new_reduce_set;
                    reduce_box = try value.Box.fixOrReduce(&self.arena.allocator, reduce_set);
                }
                return reduce_set;
            },
            .Enumerate => |body| {
                const body_set = try self.interpretExpr(body, &.{});
                var tuples = try ArrayList(value.Tuple).initCapacity(&self.arena.allocator, body_set.set.count());
                var body_iter = body_set.set.iterator();
                while (body_iter.next()) |kv| {
                    try self.interrupter.check();
                    try tuples.append(kv.key_ptr.*);
                }
                // TODO would like to be able to interrupt sorting
                std.sort.sort(value.Tuple, tuples.items, {}, struct {
                    fn lessThan(_: void, a: value.Tuple, b: value.Tuple) bool {
                        return meta.deepCompare(a, b) == .LessThan;
                    }
                }.lessThan);
                // TODO HashMap.initCapacity is private?
                var set = DeepHashSet(value.Tuple).init(&self.arena.allocator);
                for (tuples.items) |tuple, i| {
                    try self.interrupter.check();
                    var enumerated_tuple = try ArrayList(value.Scalar).initCapacity(&self.arena.allocator, 1 + tuple.len);
                    // TODO can we allocate enough tuples to overflow this conversion?
                    try enumerated_tuple.append(.{ .Number = @intToFloat(f64, i + 1) });
                    try enumerated_tuple.appendSlice(tuple);
                    _ = try set.put(enumerated_tuple.items, {});
                }
                return value.Set{
                    .arity = body_set.arity + 1,
                    .set = set,
                };
            },
            .Annotate => |annotate| {
                return self.interpretExpr(annotate.body, hint);
            },
            .Watch => |watch| {
                const result = self.interpretExpr(watch.expr, hint);
                if (result) |set| {
                    var scope = ArrayList(WatchResult.ScopeItem).init(&self.arena.allocator);
                    for (watch.scope) |scope_item|
                        try scope.append(.{
                            .name = scope_item.name,
                            .scalar = self.scope.items[self.scope.items.len - 1 - scope_item.scalar_id],
                        });
                    try self.watch_results.put(.{
                        .time = try std.mem.dupe(&self.arena.allocator, usize, self.time.items),
                        .scope = scope.toOwnedSlice(),
                        .set = set,
                    }, .{});
                } else |_| {}
                return result;
            },
            .Native => |native| {
                switch (native) {
                    .Add, .Subtract, .Multiply, .Divide, .Modulus => {
                        if (hint.len < 2) {
                            return self.setError(expr, "No hint for native arg", .{});
                        }
                        if (native == .Divide and hint[1].Number == 0) {
                            return self.setNativeError(expr, "Divide by 0", .{});
                        }
                        const result = switch (native) {
                            .Add => hint[0].Number + hint[1].Number,
                            .Subtract => hint[0].Number - hint[1].Number,
                            .Multiply => hint[0].Number * hint[1].Number,
                            .Divide => hint[0].Number / hint[1].Number,
                            .Modulus => @mod(hint[0].Number, hint[1].Number),
                            else => unreachable,
                        };
                        const tuple = try self.dupeScalars(&[_]value.Scalar{ hint[0], hint[1], .{ .Number = result } });
                        var set = DeepHashSet(value.Tuple).init(&self.arena.allocator);
                        _ = try set.put(tuple, {});
                        return value.Set{
                            .arity = 3,
                            .set = set,
                        };
                    },
                    .Range => {
                        if (hint.len < 2) {
                            return self.setError(expr, "No hint for native arg", .{});
                        }
                        const lo = @floatToInt(i64, hint[0].Number);
                        const hi = @floatToInt(i64, hint[1].Number);
                        if (@intToFloat(f64, lo) != hint[0].Number or @intToFloat(f64, hi) != hint[1].Number) {
                            return self.setNativeError(expr, "Inputs to `range` must be whole numbers, found `range {} {}`", .{ hint[0], hint[1] });
                        }
                        var i = lo;
                        var set = DeepHashSet(value.Tuple).init(&self.arena.allocator);
                        while (i <= hi) : (i += 1) {
                            try self.interrupter.check();
                            const tuple = try self.dupeScalars(&[_]value.Scalar{ hint[0], hint[1], .{ .Number = @intToFloat(f64, i) } });
                            _ = try set.put(tuple, {});
                        }
                        return value.Set{
                            .arity = 3,
                            .set = set,
                        };
                    },
                    .GreaterThan, .GreaterThanOrEqual => {
                        if (hint.len < 2) {
                            return self.setError(expr, "No hint for native arg", .{});
                        }
                        var set = DeepHashSet(value.Tuple).init(&self.arena.allocator);
                        const satisfied = switch (native) {
                            .GreaterThan => hint[0].Number > hint[1].Number,
                            .GreaterThanOrEqual => hint[0].Number >= hint[1].Number,
                            else => unreachable,
                        };
                        if (satisfied)
                            _ = try set.put(try self.dupeScalars(&.{ hint[0], hint[1] }), {});
                        return value.Set{
                            .arity = 2,
                            .set = set,
                        };
                    },
                }
            },
        }
    }

    fn interpretApply(self: *Interpreter, expr: *const core.Expr, left_set: value.Set, right_expr: *const core.Expr, hint: value.Tuple) Error!value.Set {
        var right_arity_o: ?usize = null;
        var right_set_set = DeepHashSet(value.Tuple).init(&self.arena.allocator);
        {
            var left_iter = left_set.set.iterator();
            while (left_iter.next()) |left_entry| {
                try self.interrupter.check();
                var right_hint = try std.mem.concat(
                    &self.arena.allocator,
                    value.Scalar,
                    &.{ left_entry.key_ptr.*, hint },
                );
                const right_part = try self.interpretExpr(right_expr, right_hint);
                if (right_part.set.count() > 0) {
                    if (right_arity_o) |right_arity| {
                        if (right_arity != right_part.arity) {
                            return self.setError(expr, "Apply resulted in unions over sets of different arities: {} vs {}", .{ right_arity, right_part.arity });
                        }
                    } else {
                        right_arity_o = right_part.arity;
                    }
                }
                var right_part_iter = right_part.set.iterator();
                while (right_part_iter.next()) |right_entry| {
                    try self.interrupter.check();
                    _ = try right_set_set.put(right_entry.key_ptr.*, {});
                }
            }
        }
        const right_set = value.Set{
            // TODO if right_arity is never set then arity of apply could be wrong - not sure if this matters
            .arity = right_arity_o orelse 0,
            .set = right_set_set,
        };
        const joined_arity = min(left_set.arity, right_set.arity);
        const arity = max(left_set.arity, right_set.arity) - joined_arity;
        var set = DeepHashSet(value.Tuple).init(&self.arena.allocator);
        {
            var left_iter = left_set.set.iterator();
            while (left_iter.next()) |left_entry| {
                try self.interrupter.check();
                var right_iter = right_set.set.iterator();
                while (right_iter.next()) |right_entry| {
                    try self.interrupter.check();
                    if (meta.deepEqual(left_entry.key_ptr.*[0..joined_arity], right_entry.key_ptr.*[0..joined_arity])) {
                        _ = try set.put(if (left_set.arity > right_set.arity) left_entry.key_ptr.*[joined_arity..] else right_entry.key_ptr.*[joined_arity..], {});
                    }
                }
            }
        }
        return value.Set{
            .arity = arity,
            .set = set,
        };
    }

    fn dupeScalars(self: *Interpreter, tuple: []const value.Scalar) ![]const value.Scalar {
        return std.mem.dupe(&self.arena.allocator, value.Scalar, tuple);
    }
};
