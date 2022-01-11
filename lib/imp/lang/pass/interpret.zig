const std = @import("std");
const imp = @import("../../../imp.zig");
const u = imp.util;
const syntax = imp.lang.repr.syntax;
const core = imp.lang.repr.core;
const type_ = imp.lang.repr.type_;
const value = imp.lang.repr.value;

pub fn interpret(
    arena: *u.ArenaAllocator,
    program: core.Program,
    program_type: type_.ProgramType,
    watch_results: *u.DeepHashSet(WatchResult),
    interrupter: imp.lang.Interrupter,
    error_info: *?ErrorInfo,
) Error!value.Set {
    var def_sets = try arena.allocator().alloc(u.ArrayList(Interpreter.Memo), program.defs.len);
    for (def_sets) |*def_set| def_set.* = u.ArrayList(Interpreter.Memo).init(arena.allocator());
    var interpreter = Interpreter{
        .arena = arena,
        .program = program,
        .program_type = program_type,
        .watch_results = watch_results,
        .def_sets = def_sets,
        .scope = u.ArrayList(value.Scalar).init(arena.allocator()),
        .time = u.ArrayList(usize).init(arena.allocator()),
        .interrupter = interrupter,
        .error_info = error_info,
    };
    return interpreter.interpretDef(.{ .id = program.defs.len - 1 }, &.{}, .Apply);
}

pub const Error = error{
    // sets error_info
    InterpretError,
    NativeError,

    // does not set error_info
    OutOfMemory,
    WasInterrupted,
};

pub const ErrorInfo = struct {
    expr_id: core.ExprId,
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

    pub fn dumpInto(self: WatchResult, writer: anytype, indent: u32) u.WriterError(@TypeOf(writer))!void {
        for (self.time) |time, i|
            try std.fmt.format(writer, "fix{}: {}; ", .{ i, time });
        if (self.time.len > 0) try writer.writeAll("\n");
        for (self.scope) |scope_item|
            try std.fmt.format(writer, "{s}: {}; ", .{
                scope_item.name,
                scope_item.scalar,
            });
        if (self.scope.len > 0) try writer.writeAll("\n");
        try self.set.dumpInto(writer, indent);
    }
};

// --------------------------------------------------------------------------------

const Interpreter = struct {
    arena: *u.ArenaAllocator,
    program: core.Program,
    program_type: type_.ProgramType,
    watch_results: *u.DeepHashSet(WatchResult),
    def_sets: []u.ArrayList(Memo),
    scope: u.ArrayList(value.Scalar),
    time: u.ArrayList(usize),
    interrupter: imp.lang.Interrupter,
    error_info: *?ErrorInfo,

    pub const Memo = struct {
        hint: value.Row,
        set: value.Set,
    };

    fn setError(self: *Interpreter, expr_id: core.ExprId, comptime fmt: []const u8, args: anytype) Error {
        const message = try u.formatToString(self.arena.allocator(), fmt, args);
        self.error_info.* = ErrorInfo{
            .expr_id = expr_id,
            .message = message,
        };
        return error.InterpretError;
    }

    fn setNativeError(self: *Interpreter, expr_id: core.ExprId, comptime fmt: []const u8, args: anytype) Error {
        const message = try u.formatToString(self.arena.allocator(), fmt, args);
        self.error_info.* = ErrorInfo{
            .expr_id = expr_id,
            .message = message,
        };
        return error.NativeError;
    }

    fn interpretDef(self: *Interpreter, def_id: core.DefId, hint: value.Row, hint_mode: type_.HintMode) Error!value.Set {
        const def_set = &self.def_sets[def_id.id];

        // check if already evaluated
        // TODO it's surprisingly hard to write a better lookup than this
        for (def_set.items) |memo|
            if (u.deepEqual(memo.hint, hint))
                return memo.set;

        // otherwise, evaluate
        const old_scope = self.scope;
        defer self.scope = old_scope;
        self.scope = u.ArrayList(value.Scalar).init(self.arena.allocator());
        //TODO need to preserve time for watches, but does correctness ever depend on resetting time?
        //const old_time = self.time;
        //defer self.time = old_time;
        //self.time = u.ArrayList(usize).init(self.arena.allocator());
        const set = try self.interpretExpr(self.program.defs[def_id.id], hint, hint_mode);

        // memoize
        try def_set.append(.{ .hint = hint, .set = set });

        return set;
    }

    fn interpretBox(self: *Interpreter, box: core.Box, hint: value.Row, hint_mode: type_.HintMode) Error!value.Set {
        const box_args = try self.arena.allocator().alloc(value.Scalar, box.args.len);
        for (box_args) |*box_arg, i|
            box_arg.* = self.scope.items[self.scope.items.len - 1 - box.args[i].id];
        const box_hint = try std.mem.concat(self.arena.allocator(), value.Scalar, &.{ box_args, hint });
        const set = try self.interpretDef(box.def_id, box_hint, hint_mode);
        var result_set = value.Set{
            .set = u.DeepHashSet(value.Row).init(self.arena.allocator()),
        };
        var set_iter = set.set.iterator();
        while (set_iter.next()) |entry|
            if (u.deepEqual(entry.key_ptr.*[0..box.args.len], box_args))
                try result_set.set.put(entry.key_ptr.*[box.args.len..], {});
        return result_set;
    }

    fn interpretExpr(self: *Interpreter, expr_id: core.ExprId, hint: value.Row, hint_mode: type_.HintMode) Error!value.Set {
        try self.interrupter.check();
        const expr = self.program.exprs[expr_id.id];
        switch (expr) {
            .None => {
                const set = u.DeepHashSet(value.Row).init(self.arena.allocator());
                return value.Set{
                    .set = set,
                };
            },
            .Some => {
                var set = u.DeepHashSet(value.Row).init(self.arena.allocator());
                _ = try set.put(&.{}, {});
                return value.Set{
                    .set = set,
                };
            },
            .Scalar => |scalar| {
                var set = u.DeepHashSet(value.Row).init(self.arena.allocator());
                _ = try set.put(try self.dupeScalars(&.{scalar}), {});
                return value.Set{
                    .set = set,
                };
            },
            .Union => |pair| {
                const left = try self.interpretExpr(pair.left, hint, hint_mode);
                const right = try self.interpretExpr(pair.right, hint, hint_mode);
                var set = u.DeepHashSet(value.Row).init(self.arena.allocator());
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
                return value.Set{ .set = set };
            },
            .Intersect => |pair| {
                if (self.interpretExpr(pair.left, hint, hint_mode)) |left| {
                    return self.interpretIntersect(left, pair.right);
                } else |_| {
                    if (self.interpretExpr(pair.right, hint, hint_mode)) |right| {
                        return self.interpretIntersect(right, pair.left);
                    } else |err| {
                        return err;
                    }
                }
            },
            .Product => |pair| {
                const left = try self.interpretExpr(pair.left, hint, hint_mode);
                var set = u.DeepHashSet(value.Row).init(self.arena.allocator());
                const right_hint_start = switch (left.getArity()) {
                    .Unknown => return value.Set{ .set = set },
                    .Known => |known| u.min(hint.len, known),
                    .Mixed => hint.len, // no hint available
                };
                const right = try self.interpretExpr(pair.right, hint[right_hint_start..], hint_mode);
                var left_iter = left.set.iterator();
                while (left_iter.next()) |lkv| {
                    try self.interrupter.check();
                    var right_iter = right.set.iterator();
                    while (right_iter.next()) |rkv| {
                        try self.interrupter.check();
                        var row = try self.arena.allocator().alloc(value.Scalar, lkv.key_ptr.len + rkv.key_ptr.len);
                        var i: usize = 0;
                        for (lkv.key_ptr.*) |scalar| {
                            row[i] = scalar;
                            i += 1;
                        }
                        for (rkv.key_ptr.*) |scalar| {
                            row[i] = scalar;
                            i += 1;
                        }
                        _ = try set.put(row, {});
                    }
                }
                return value.Set{
                    .set = set,
                };
            },
            .Equal => |pair| {
                // the hint for expr doesn't tell us anything about left or right
                const left = try self.interpretExpr(pair.left, &.{}, .Apply);
                const right = try self.interpretExpr(pair.right, &.{}, .Apply);
                var set = u.DeepHashSet(value.Row).init(self.arena.allocator());
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
                    .set = set,
                };
            },
            .ScalarId => |scalar_id| {
                var set = u.DeepHashSet(value.Row).init(self.arena.allocator());
                const scalar = self.scope.items[self.scope.items.len - 1 - scalar_id.id];
                _ = try set.put(try self.dupeScalars(&.{scalar}), {});
                return value.Set{
                    .set = set,
                };
            },
            .UnboxScalarId => |scalar_id| {
                const scalar = self.scope.items[self.scope.items.len - 1 - scalar_id.id];
                switch (scalar) {
                    .Box => |box| {
                        switch (box) {
                            .Normal => |normal| {
                                const box_hint = try std.mem.concat(
                                    self.arena.allocator(),
                                    value.Scalar,
                                    &.{
                                        normal.args,
                                        hint,
                                    },
                                );
                                const set = try self.interpretDef(normal.def_id, box_hint, hint_mode);
                                var result_set = value.Set{
                                    .set = u.DeepHashSet(value.Row).init(self.arena.allocator()),
                                };
                                var set_iter = set.set.iterator();
                                while (set_iter.next()) |entry|
                                    if (u.deepEqual(entry.key_ptr.*[0..normal.args.len], normal.args))
                                        try result_set.set.put(entry.key_ptr.*[normal.args.len..], {});
                                return result_set;
                            },
                            .FixOrReduce => |_| return box.getFixOrReduce(),
                        }
                    },
                    else => {
                        return self.setError(expr_id, "Tried to unbox {} which is not a box", .{scalar});
                    },
                }
            },
            .DefId => |def_id| return self.interpretDef(def_id, hint, hint_mode),
            .Negate => |body| {
                // the hint for expr doesn't tell us anything about body
                const body_set = try self.interpretExpr(body, &.{}, hint_mode);
                var set = u.DeepHashSet(value.Row).init(self.arena.allocator());
                if (body_set.set.count() == 0) {
                    _ = try set.put(&.{}, {});
                }
                return value.Set{
                    .set = set,
                };
            },
            .Then => |then| {
                // the hint for expr doesn't tell us anything about condition
                const condition = try self.interpretExpr(then.condition, &.{}, hint_mode);
                if (condition.set.count() == 0) {
                    const set = u.DeepHashSet(value.Row).init(self.arena.allocator());
                    return value.Set{
                        .set = set,
                    };
                } else {
                    return self.interpretExpr(then.true_branch, hint, hint_mode);
                }
            },
            .Abstract => |body| {
                if (hint.len == 0) {
                    switch (hint_mode) {
                        .Apply => return self.setError(expr_id, "No hint for arg", .{}),
                        .Intersect => {
                            const set = u.DeepHashSet(value.Row).init(self.arena.allocator());
                            return value.Set{
                                .set = set,
                            };
                        },
                    }
                } else {
                    try self.scope.append(hint[0]);
                    const body_set = try self.interpretExpr(body, hint[1..], hint_mode);
                    _ = self.scope.pop();
                    var set = u.DeepHashSet(value.Row).init(self.arena.allocator());
                    var body_set_iter = body_set.set.iterator();
                    while (body_set_iter.next()) |kv| {
                        try self.interrupter.check();
                        var row = try u.ArrayList(value.Scalar).initCapacity(self.arena.allocator(), kv.key_ptr.len + 1);
                        try row.append(hint[0]);
                        try row.appendSlice(kv.key_ptr.*);
                        _ = try set.put(row.items, {});
                    }
                    return value.Set{
                        .set = set,
                    };
                }
            },
            .Apply => |pair| {
                // can't make use of hint until we know which side is finite
                if (self.interpretExpr(pair.left, &.{}, .Apply)) |left_type| {
                    return self.interpretApply(left_type, pair.right, hint, hint_mode);
                } else |_| {
                    // error might have been from lack of hints, so try other way around
                    // TODO could this cause exponential retries in large program?
                    if (self.interpretExpr(pair.right, &.{}, .Apply)) |right_type| {
                        return self.interpretApply(right_type, pair.left, hint, hint_mode);
                    } else |err| {
                        return err;
                    }
                }
            },
            .Box => |box| {
                var args = try self.arena.allocator().alloc(value.Scalar, box.args.len);
                for (box.args) |scalar_id, i| {
                    args[i] = self.scope.items[self.scope.items.len - 1 - scalar_id.id];
                }
                const scalar = value.Box{
                    .Normal = .{
                        .def_id = box.def_id,
                        .args = args,
                    },
                };
                const row = try self.dupeScalars(&[1]value.Scalar{.{ .Box = scalar }});
                var set = u.DeepHashSet(value.Row).init(self.arena.allocator());
                _ = try set.put(row, {});
                return value.Set{
                    .set = set,
                };
            },
            .Fix => |fix| {
                try self.time.append(0);
                const init_set = try self.interpretExpr(fix.init, &.{}, .Apply);
                _ = self.time.pop();
                const init_box = try value.Box.fixOrReduce(self.arena.allocator(), init_set);

                var fix_hint = try self.arena.allocator().alloc(value.Scalar, 1);

                var fix_box = init_box;
                var fix_set = init_set;
                var iteration: usize = 1;
                while (true) : (iteration += 1) {
                    try self.interrupter.check();
                    try self.time.append(iteration);
                    defer _ = self.time.pop();
                    fix_hint[0] = .{ .Box = fix_box };
                    const body_set = try self.interpretBox(fix.next, fix_hint, .Apply);
                    var new_fix_set = value.Set{
                        .set = u.DeepHashSet(value.Row).init(self.arena.allocator()),
                    };
                    var body_iter = body_set.set.iterator();
                    while (body_iter.next()) |kv| {
                        try self.interrupter.check();
                        if (u.deepEqual(kv.key_ptr.*[0], value.Scalar{ .Box = fix_box })) {
                            _ = try new_fix_set.set.put(kv.key_ptr.*[1..], {});
                        }
                    }
                    if (u.deepEqual(fix_set, new_fix_set)) {
                        return fix_set;
                    }
                    fix_set = new_fix_set;
                    fix_box = try value.Box.fixOrReduce(self.arena.allocator(), fix_set);
                }
            },
            .Reduce => |reduce| {
                const input_set = try self.interpretExpr(reduce.input, &.{}, .Apply);
                var input_rows = try u.ArrayList(value.Row).initCapacity(self.arena.allocator(), input_set.set.count());
                var input_iter = input_set.set.iterator();
                while (input_iter.next()) |kv| {
                    try self.interrupter.check();
                    try input_rows.append(kv.key_ptr.*);
                }
                // TODO would like to be able to interrupt sorting
                std.sort.sort(value.Row, input_rows.items, {}, struct {
                    fn lessThan(_: void, a: value.Row, b: value.Row) bool {
                        return u.deepCompare(a, b) == .LessThan;
                    }
                }.lessThan);

                try self.time.append(0);
                const init_set = try self.interpretExpr(reduce.init, &.{}, .Apply);
                _ = self.time.pop();
                const init_box = try value.Box.fixOrReduce(self.arena.allocator(), init_set);

                var reduce_hint = try self.arena.allocator().alloc(value.Scalar, 2);

                var reduce_box = init_box;
                var reduce_set = init_set;
                for (input_rows.items) |input_row, iteration| {
                    try self.interrupter.check();
                    try self.time.append(iteration);
                    defer _ = self.time.pop();
                    var row_set = value.Set{
                        .set = u.DeepHashSet(value.Row).init(self.arena.allocator()),
                    };
                    _ = try row_set.set.put(input_row, {});
                    const row_box = try value.Box.fixOrReduce(self.arena.allocator(), row_set);
                    reduce_hint[0] = .{ .Box = reduce_box };
                    reduce_hint[1] = .{ .Box = row_box };
                    const body_set = try self.interpretBox(reduce.next, reduce_hint, .Apply);
                    var new_reduce_set = value.Set{
                        .set = u.DeepHashSet(value.Row).init(self.arena.allocator()),
                    };
                    var body_iter = body_set.set.iterator();
                    while (body_iter.next()) |kv| {
                        try self.interrupter.check();
                        if (u.deepEqual(kv.key_ptr.*[0], value.Scalar{ .Box = reduce_box }) and u.deepEqual(kv.key_ptr.*[1], value.Scalar{ .Box = row_box })) {
                            _ = try new_reduce_set.set.put(kv.key_ptr.*[2..], {});
                        }
                    }
                    reduce_set = new_reduce_set;
                    reduce_box = try value.Box.fixOrReduce(self.arena.allocator(), reduce_set);
                }
                return reduce_set;
            },
            .Enumerate => |body| {
                const body_set = try self.interpretExpr(body, &.{}, .Apply);
                var rows = try u.ArrayList(value.Row).initCapacity(self.arena.allocator(), body_set.set.count());
                var body_iter = body_set.set.iterator();
                while (body_iter.next()) |kv| {
                    try self.interrupter.check();
                    try rows.append(kv.key_ptr.*);
                }
                // TODO would like to be able to interrupt sorting
                std.sort.sort(value.Row, rows.items, {}, struct {
                    fn lessThan(_: void, a: value.Row, b: value.Row) bool {
                        return u.deepCompare(a, b) == .LessThan;
                    }
                }.lessThan);
                // TODO HashMap.initCapacity is private?
                var set = u.DeepHashSet(value.Row).init(self.arena.allocator());
                for (rows.items) |row, i| {
                    try self.interrupter.check();
                    var enumerated_row = try u.ArrayList(value.Scalar).initCapacity(self.arena.allocator(), 1 + row.len);
                    // TODO can we allocate enough rows to overflow this conversion?
                    try enumerated_row.append(.{ .Number = @intToFloat(f64, i + 1) });
                    try enumerated_row.appendSlice(row);
                    _ = try set.put(enumerated_row.items, {});
                }
                return value.Set{
                    .set = set,
                };
            },
            .Annotate => |annotate| {
                return self.interpretExpr(annotate.body, hint, hint_mode);
            },
            .Watch => |watch| {
                const result = self.interpretExpr(watch.body, hint, hint_mode);
                if (result) |set| {
                    var scope = u.ArrayList(WatchResult.ScopeItem).init(self.arena.allocator());
                    for (watch.scope) |scope_item|
                        try scope.append(.{
                            .name = scope_item.name,
                            .scalar = self.scope.items[self.scope.items.len - 1 - scope_item.scalar_id.id],
                        });
                    try self.watch_results.put(.{
                        .time = try self.arena.allocator().dupe(usize, self.time.items),
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
                            return self.setError(expr_id, "No hint for native arg", .{});
                        }
                        if (native == .Divide and hint[1].Number == 0) {
                            return self.setNativeError(expr_id, "Divide by 0", .{});
                        }
                        var set = u.DeepHashSet(value.Row).init(self.arena.allocator());
                        if (hint[0] == .Number and hint[1] == .Number) {
                            const result = switch (native) {
                                .Add => hint[0].Number + hint[1].Number,
                                .Subtract => hint[0].Number - hint[1].Number,
                                .Multiply => hint[0].Number * hint[1].Number,
                                .Divide => hint[0].Number / hint[1].Number,
                                .Modulus => @mod(hint[0].Number, hint[1].Number),
                                else => unreachable,
                            };
                            const row = try self.dupeScalars(&[_]value.Scalar{ hint[0], hint[1], .{ .Number = result } });
                            _ = try set.put(row, {});
                        }
                        return value.Set{
                            .set = set,
                        };
                    },
                    .Range => {
                        if (hint.len < 2) {
                            return self.setError(expr_id, "No hint for native arg", .{});
                        }
                        var set = u.DeepHashSet(value.Row).init(self.arena.allocator());
                        if (hint[0] == .Number and hint[1] == .Number) {
                            const lo = @floatToInt(i64, hint[0].Number);
                            const hi = @floatToInt(i64, hint[1].Number);
                            if (@intToFloat(f64, lo) != hint[0].Number or @intToFloat(f64, hi) != hint[1].Number) {
                                return self.setNativeError(expr_id, "Inputs to `range` must be whole numbers, found `range {} {}`", .{ hint[0], hint[1] });
                            }
                            var i = lo;
                            while (i <= hi) : (i += 1) {
                                try self.interrupter.check();
                                const row = try self.dupeScalars(&[_]value.Scalar{ hint[0], hint[1], .{ .Number = @intToFloat(f64, i) } });
                                _ = try set.put(row, {});
                            }
                        }
                        return value.Set{
                            .set = set,
                        };
                    },
                    .GreaterThan, .GreaterThanOrEqual => {
                        if (hint.len < 2) {
                            return self.setError(expr_id, "No hint for native arg", .{});
                        }
                        var set = u.DeepHashSet(value.Row).init(self.arena.allocator());
                        if (hint[0] == .Number and hint[1] == .Number) {
                            const satisfied = switch (native) {
                                .GreaterThan => hint[0].Number > hint[1].Number,
                                .GreaterThanOrEqual => hint[0].Number >= hint[1].Number,
                                else => unreachable,
                            };
                            if (satisfied)
                                _ = try set.put(try self.dupeScalars(&.{ hint[0], hint[1] }), {});
                        }
                        return value.Set{
                            .set = set,
                        };
                    },
                    .Number, .Text => {
                        var set = u.DeepHashSet(value.Row).init(self.arena.allocator());
                        if (hint.len < 1)
                            return switch (hint_mode) {
                                .Apply => self.setError(expr_id, "Could not infer the type of abstract arg", .{}),
                                .Intersect => return value.Set{
                                    .set = set,
                                },
                            };
                        const satisfied =
                            switch (native) {
                            .Number => hint[0] == .Number,
                            .Text => hint[0] == .Text,
                            else => unreachable,
                        };
                        if (satisfied)
                            _ = try set.put(try self.dupeScalars(&.{hint[0]}), {});
                        return value.Set{
                            .set = set,
                        };
                    },
                }
            },
            .IsTest => |pair| {
                const left_set = try self.interpretExpr(pair.left, &.{}, .Apply);
                const left_and_right_set = try self.interpretIntersect(left_set, pair.right);
                var set = u.DeepHashSet(value.Row).init(self.arena.allocator());
                if (u.deepEqual(left_set, left_and_right_set))
                    _ = try set.put(&.{}, {});
                return value.Set{
                    .set = set,
                };
            },
            .IsAssert => |pair| {
                const left_set = try self.interpretExpr(pair.left, &.{}, .Apply);
                const left_and_right_set = try self.interpretIntersect(left_set, pair.right);
                return if (u.deepEqual(left_set, left_and_right_set))
                    left_set
                else
                    self.setError(expr_id, "Assert failed: {} is not contained in right.", .{left_set});
            },
            .As => |pair| {
                const left_set = try self.interpretExpr(pair.left, &.{}, .Apply);
                return self.interpretIntersect(left_set, pair.right);
            },
        }
    }

    fn interpretIntersect(self: *Interpreter, left_set: value.Set, right_expr_id: core.ExprId) Error!value.Set {
        var set = u.DeepHashSet(value.Row).init(self.arena.allocator());
        var left_iter = left_set.set.keyIterator();
        while (left_iter.next()) |left_row| {
            try self.interrupter.check();
            const right = try self.interpretExpr(right_expr_id, left_row.*, .Intersect);
            if (right.set.contains(left_row.*)) {
                _ = try set.put(left_row.*, {});
            }
        }
        return value.Set{
            .set = set,
        };
    }

    fn interpretApply(self: *Interpreter, left_set: value.Set, right_expr_id: core.ExprId, hint: value.Row, hint_mode: type_.HintMode) Error!value.Set {
        var right_set_set = u.DeepHashSet(value.Row).init(self.arena.allocator());
        {
            var left_iter = left_set.set.iterator();
            while (left_iter.next()) |left_entry| {
                try self.interrupter.check();
                const right_hint = try std.mem.concat(
                    self.arena.allocator(),
                    value.Scalar,
                    &.{ left_entry.key_ptr.*, hint },
                );
                const right_part = try self.interpretExpr(right_expr_id, right_hint, hint_mode);
                var right_part_iter = right_part.set.iterator();
                while (right_part_iter.next()) |right_entry| {
                    try self.interrupter.check();
                    _ = try right_set_set.put(right_entry.key_ptr.*, {});
                }
            }
        }
        const right_set = value.Set{
            .set = right_set_set,
        };
        var set = u.DeepHashSet(value.Row).init(self.arena.allocator());
        {
            var left_iter = left_set.set.iterator();
            while (left_iter.next()) |left_entry| {
                try self.interrupter.check();
                var right_iter = right_set.set.iterator();
                while (right_iter.next()) |right_entry| {
                    try self.interrupter.check();
                    const joined_arity = u.min(left_entry.key_ptr.len, right_entry.key_ptr.len);
                    if (u.deepEqual(left_entry.key_ptr.*[0..joined_arity], right_entry.key_ptr.*[0..joined_arity])) {
                        _ = try set.put(if (left_entry.key_ptr.len > right_entry.key_ptr.len) left_entry.key_ptr.*[joined_arity..] else right_entry.key_ptr.*[joined_arity..], {});
                    }
                }
            }
        }
        return value.Set{
            .set = set,
        };
    }

    fn dupeScalars(self: *Interpreter, row: []const value.Scalar) ![]const value.Scalar {
        return self.arena.allocator().dupe(value.Scalar, row);
    }
};
