const imp = @import("../../../imp.zig");
usingnamespace imp.common;
const meta = imp.meta;
const Store = imp.lang.Store;
const core = imp.lang.repr.core;
const syntax = imp.lang.repr.syntax;
const value = imp.lang.repr.value;

/// Guarantees:
/// * If expr typechecks then this will not return InterpretError
/// * If expr has a finite type then this will return a value.Set.Finite
pub fn interpret(
    store: *const Store,
    arena: *ArenaAllocator,
    expr: *const core.Expr,
    watch_expr_o: ?*const core.Expr,
    watch_results: *DeepHashSet(WatchResult),
    interrupter: imp.lang.Interrupter,
    error_info: *?ErrorInfo,
) Error!value.Set {
    var interpreter = Interpreter{
        .store = store,
        .arena = arena,
        .watch_expr_o = watch_expr_o,
        .watch_results = watch_results,
        .scope = ArrayList(value.Scalar).init(&store.arena.allocator),
        .time = ArrayList(value.Time).init(&store.arena.allocator),
        .boxes = DeepHashMap(value.LazySet, value.Set).init(&store.arena.allocator),
        .interrupter = interrupter,
        .error_info = error_info,
    };
    return interpreter.interpret(expr, &[0]value.Scalar{});
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
    expr: *const core.Expr,
    message: []const u8,
};

pub const WatchResult = struct {
    time: []const value.Time,
    scope: []const ArgAndScalar,
    set: value.Set,
};

pub const ArgAndScalar = struct {
    arg: syntax.Arg,
    scalar: value.Scalar,
};

// --------------------------------------------------------------------------------

const Interpreter = struct {
    store: *const Store,
    arena: *ArenaAllocator,
    watch_expr_o: ?*const core.Expr,
    watch_results: *DeepHashSet(WatchResult),
    scope: ArrayList(value.Scalar),
    time: ArrayList(value.Time),
    boxes: DeepHashMap(value.LazySet, value.Set),
    interrupter: imp.lang.Interrupter,
    error_info: *?ErrorInfo,

    fn setError(self: *Interpreter, expr: *const core.Expr, comptime fmt: []const u8, args: anytype) Error {
        const message = try format(&self.arena.allocator, fmt, args);
        self.error_info.* = ErrorInfo{
            .expr = expr,
            .message = message,
        };
        return error.InterpretError;
    }

    fn setNativeError(self: *Interpreter, expr: *const core.Expr, comptime fmt: []const u8, args: anytype) Error {
        const message = try format(&self.arena.allocator, fmt, args);
        self.error_info.* = ErrorInfo{
            .expr = expr,
            .message = message,
        };
        return error.NativeError;
    }

    fn interpret(self: *Interpreter, expr: *const core.Expr, hint: value.Tuple) Error!value.Set {
        const result = self.interpretInner(expr, hint);
        if (self.watch_expr_o) |watch_expr|
            if (expr == watch_expr)
                if (result) |set| {
                    var scope = ArrayList(ArgAndScalar).init(&self.arena.allocator);
                    const watch_meta = Store.getCoreMeta(expr);
                    for (watch_meta.scope) |arg_o, i|
                        if (arg_o) |arg|
                            try scope.append(.{ .arg = arg, .scalar = self.scope.items[i] });
                    try self.watch_results.put(.{
                        .time = try std.mem.dupe(&self.arena.allocator, value.Time, self.time.items),
                        .scope = scope.toOwnedSlice(),
                        .set = set,
                    }, .{});
                } else |_| {};
        return result;
    }

    fn interpretInner(self: *Interpreter, expr: *const core.Expr, hint: value.Tuple) Error!value.Set {
        try self.interrupter.check();
        switch (expr.*) {
            .None => {
                const set = DeepHashSet(value.Tuple).init(&self.arena.allocator);
                return value.Set{
                    .Finite = .{
                        .arity = 0,
                        .set = set,
                    },
                };
            },
            .Some => {
                var set = DeepHashSet(value.Tuple).init(&self.arena.allocator);
                _ = try set.put(&[0]value.Scalar{}, {});
                return value.Set{
                    .Finite = .{
                        .arity = 0,
                        .set = set,
                    },
                };
            },
            .Scalar => |scalar| {
                var set = DeepHashSet(value.Tuple).init(&self.arena.allocator);
                _ = try set.put(try self.dupeScalars(&[1]value.Scalar{scalar}), {});
                return value.Set{
                    .Finite = .{
                        .arity = 1,
                        .set = set,
                    },
                };
            },
            .Union => |pair| {
                const left = try self.interpret(pair.left, hint);
                const right = try self.interpret(pair.right, hint);
                if (left == .Lazy or right == .Lazy) {
                    return value.Set{
                        .Lazy = .{
                            .expr = expr,
                            .scope = try self.dupeScalars(self.scope.items),
                            .time = try self.dupeTime(self.time.items),
                        },
                    };
                }
                if (left.Finite.arity != right.Finite.arity and left.Finite.set.count() > 0 and right.Finite.set.count() > 0) {
                    return self.setError(expr, "Tried to union sets with different arities: {} vs {}", .{ left.Finite.arity, right.Finite.arity });
                }
                var set = DeepHashSet(value.Tuple).init(&self.arena.allocator);
                var left_iter = left.Finite.set.iterator();
                while (left_iter.next()) |kv| {
                    try self.interrupter.check();
                    _ = try set.put(kv.key_ptr.*, {});
                }
                var right_iter = right.Finite.set.iterator();
                while (right_iter.next()) |kv| {
                    try self.interrupter.check();
                    _ = try set.put(kv.key_ptr.*, {});
                }
                return value.Set{
                    .Finite = .{
                        .arity = max(left.Finite.arity, right.Finite.arity),
                        .set = set,
                    },
                };
            },
            .Intersect => |pair| {
                const left = try self.interpret(pair.left, hint);
                const right = try self.interpret(pair.right, hint);
                if (left == .Lazy or right == .Lazy) {
                    return value.Set{
                        .Lazy = .{
                            .expr = expr,
                            .scope = try self.dupeScalars(self.scope.items),
                            .time = try self.dupeTime(self.time.items),
                        },
                    };
                }
                if (left.Finite.arity != right.Finite.arity and left.Finite.set.count() > 0 and right.Finite.set.count() > 0) {
                    return self.setError(expr, "Tried to intersect sets with different arities: {} vs {}", .{ left.Finite.arity, right.Finite.arity });
                }
                var set = DeepHashSet(value.Tuple).init(&self.arena.allocator);
                var left_iter = left.Finite.set.iterator();
                while (left_iter.next()) |kv| {
                    try self.interrupter.check();
                    if (right.Finite.set.contains(kv.key_ptr.*)) {
                        _ = try set.put(kv.key_ptr.*, {});
                    }
                }
                return value.Set{
                    .Finite = .{
                        .arity = max(left.Finite.arity, right.Finite.arity),
                        .set = set,
                    },
                };
            },
            .Product => |pair| {
                const left = try self.interpret(pair.left, hint);
                if (left == .Lazy) {
                    return value.Set{
                        .Lazy = .{
                            .expr = expr,
                            .scope = try self.dupeScalars(self.scope.items),
                            .time = try self.dupeTime(self.time.items),
                        },
                    };
                }
                const right = try self.interpret(pair.right, hint[min(hint.len, left.Finite.arity)..]);
                if (right == .Lazy) {
                    return value.Set{
                        .Lazy = .{
                            .expr = expr,
                            .scope = try self.dupeScalars(self.scope.items),
                            .time = try self.dupeTime(self.time.items),
                        },
                    };
                }
                var set = DeepHashSet(value.Tuple).init(&self.arena.allocator);
                var left_iter = left.Finite.set.iterator();
                while (left_iter.next()) |lkv| {
                    try self.interrupter.check();
                    var right_iter = right.Finite.set.iterator();
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
                    .Finite = .{
                        .arity = left.Finite.arity + right.Finite.arity,
                        .set = set,
                    },
                };
            },
            .Equal => |pair| {
                // the hint for expr doesn't tell us anything about left or right
                const left = try self.interpret(pair.left, &[0]value.Scalar{});
                const right = try self.interpret(pair.right, &[0]value.Scalar{});
                if (left == .Lazy or right == .Lazy) {
                    return self.setError(expr, "Cannot equal one or more lazy sets", .{});
                }
                var set = DeepHashSet(value.Tuple).init(&self.arena.allocator);
                const isEqual = isEqual: {
                    var leftIter = left.Finite.set.iterator();
                    while (leftIter.next()) |kv| {
                        try self.interrupter.check();
                        if (!right.Finite.set.contains(kv.key_ptr.*)) {
                            break :isEqual false;
                        }
                    }
                    var rightIter = right.Finite.set.iterator();
                    while (rightIter.next()) |kv| {
                        try self.interrupter.check();
                        if (!left.Finite.set.contains(kv.key_ptr.*)) {
                            break :isEqual false;
                        }
                    }
                    break :isEqual true;
                };
                if (isEqual) {
                    _ = try set.put(&[0]value.Scalar{}, {});
                }
                return value.Set{
                    .Finite = .{
                        .arity = 0,
                        .set = set,
                    },
                };
            },
            .Name => |name_ix| {
                var set = DeepHashSet(value.Tuple).init(&self.arena.allocator);
                const scalar = self.scope.items[self.scope.items.len - 1 - name_ix];
                _ = try set.put(try self.dupeScalars(&[1]value.Scalar{scalar}), {});
                return value.Set{
                    .Finite = .{
                        .arity = 1,
                        .set = set,
                    },
                };
            },
            .UnboxName => |name_ix| {
                const scalar = self.scope.items[self.scope.items.len - 1 - name_ix];
                switch (scalar) {
                    .Box => |box| {
                        if (self.boxes.get(box)) |set| {
                            switch (set) {
                                .Lazy => |lazy| {
                                    // try to specialize
                                    const old_scope = self.scope;
                                    defer self.scope = old_scope;
                                    self.scope = try ArrayList(value.Scalar).initCapacity(&self.store.arena.allocator, lazy.scope.len);
                                    try self.scope.appendSlice(lazy.scope);
                                    const old_time = self.time;
                                    defer self.time = old_time;
                                    self.time = try ArrayList(value.Time).initCapacity(&self.store.arena.allocator, lazy.time.len);
                                    try self.time.appendSlice(lazy.time);
                                    return self.interpret(lazy.expr, hint);
                                },
                                .Finite => {
                                    return set;
                                },
                            }
                        } else {
                            imp_panic("No box for {}", .{box});
                        }
                    },
                    else => {
                        return self.setError(expr, "Tried to unbox {} which is not a box", .{scalar});
                    },
                }
            },
            .Negate => |body| {
                // the hint for expr doesn't tell us anything about body
                const body_set = try self.interpret(body, &[0]value.Scalar{});
                if (body_set == .Lazy) {
                    return self.setError(expr, "The body of `!` cannot be a lazy set", .{});
                }
                var set = DeepHashSet(value.Tuple).init(&self.arena.allocator);
                if (body_set.Finite.set.count() == 0) {
                    _ = try set.put(&[0]value.Scalar{}, {});
                }
                return value.Set{
                    .Finite = .{
                        .arity = 0,
                        .set = set,
                    },
                };
            },
            .Then => |then| {
                // the hint for expr doesn't tell us anything about condition
                const condition = try self.interpret(then.condition, &[0]value.Scalar{});
                if (condition == .Lazy) {
                    return self.setError(expr, "The condition for `then` cannot be a lazy set", .{});
                }
                if (condition.Finite.set.count() == 0) {
                    const set = DeepHashSet(value.Tuple).init(&self.arena.allocator);
                    return value.Set{
                        .Finite = .{
                            .arity = 0,
                            .set = set,
                        },
                    };
                } else {
                    return self.interpret(then.true_branch, hint);
                }
            },
            .Abstract => |body| {
                if (hint.len == 0) {
                    return value.Set{
                        .Lazy = .{
                            .expr = expr,
                            .scope = try self.dupeScalars(self.scope.items),
                            .time = try self.dupeTime(self.time.items),
                        },
                    };
                } else {
                    // if we have a hint we can try to specialize the body
                    try self.scope.append(hint[0]);
                    const body_set = try self.interpret(body, hint[1..]);
                    _ = self.scope.pop();
                    switch (body_set) {
                        .Finite => |finite| {
                            const arity = finite.arity + 1;
                            var set = DeepHashSet(value.Tuple).init(&self.arena.allocator);
                            var finite_iter = finite.set.iterator();
                            while (finite_iter.next()) |kv| {
                                try self.interrupter.check();
                                var tuple = try ArrayList(value.Scalar).initCapacity(&self.arena.allocator, kv.key_ptr.len + 1);
                                try tuple.append(hint[0]);
                                try tuple.appendSlice(kv.key_ptr.*);
                                _ = try set.put(tuple.items, {});
                            }
                            return value.Set{
                                .Finite = .{
                                    .arity = arity,
                                    .set = set,
                                },
                            };
                        },
                        .Lazy => {
                            // couldn't fully specialize, give up
                            return value.Set{
                                .Lazy = .{
                                    .expr = expr,
                                    .scope = try self.dupeScalars(self.scope.items),
                                    .time = try self.dupeTime(self.time.items),
                                },
                            };
                        },
                    }
                }
            },
            .Apply => |pair| {
                // analyze without hints first because we don't know how to split the hint between left and right
                var pair_left = pair.left;
                var pair_right = pair.right;
                var left = try self.interpret(pair.left, &[0]value.Scalar{});
                var right = try self.interpret(pair.right, &[0]value.Scalar{});
                if (left == .Lazy and right == .Lazy) {
                    return self.setError(expr, "Cannot apply two lazy sets", .{});
                }
                if (right == .Lazy) {
                    std.mem.swap(*const core.Expr, &pair_left, &pair_right);
                    std.mem.swap(value.Set, &left, &right);
                }
                if (left == .Lazy) {
                    // try again but with hints this time
                    var left_arity_o: ?usize = null;
                    var left_set = DeepHashSet(value.Tuple).init(&self.arena.allocator);
                    var right_iter = right.Finite.set.iterator();
                    while (right_iter.next()) |kv| {
                        try self.interrupter.check();
                        var left_hint = try ArrayList(value.Scalar).initCapacity(&self.arena.allocator, right.Finite.arity + hint.len);
                        try left_hint.appendSlice(kv.key_ptr.*);
                        try left_hint.appendSlice(hint);
                        const left_part = try self.interpret(pair_left, left_hint.items);
                        if (left_part == .Lazy) {
                            // couldn't fully specialize, give up
                            return value.Set{
                                .Lazy = .{
                                    .expr = expr,
                                    .scope = try self.dupeScalars(self.scope.items),
                                    .time = try self.dupeTime(self.time.items),
                                },
                            };
                        }
                        if (left_part.Finite.set.count() > 0) {
                            if (left_arity_o) |left_arity| {
                                if (left_arity != left_part.Finite.arity) {
                                    return self.setError(expr, "Apply resulted in unions over sets of different arities: {} vs {}", .{ left_arity, left_part.Finite.arity });
                                }
                            } else {
                                left_arity_o = left_part.Finite.arity;
                            }
                        }
                        var left_part_iter = left_part.Finite.set.iterator();
                        while (left_part_iter.next()) |lkv| {
                            try self.interrupter.check();
                            _ = try left_set.put(lkv.key_ptr.*, {});
                        }
                    }
                    left = .{
                        .Finite = .{
                            // TODO if left_arity is never set then arity of apply could be wrong - not sure if this matters
                            .arity = left_arity_o orelse 0,
                            .set = left_set,
                        },
                    };
                }
                const joined_arity = min(left.Finite.arity, right.Finite.arity);
                const arity = max(left.Finite.arity, right.Finite.arity) - joined_arity;
                var set = DeepHashSet(value.Tuple).init(&self.arena.allocator);
                var left_iter = left.Finite.set.iterator();
                while (left_iter.next()) |lkv| {
                    try self.interrupter.check();
                    var right_iter = right.Finite.set.iterator();
                    while (right_iter.next()) |rkv| {
                        try self.interrupter.check();
                        if (meta.deepEqual(lkv.key_ptr.*[0..joined_arity], rkv.key_ptr.*[0..joined_arity])) {
                            _ = try set.put(if (left.Finite.arity > right.Finite.arity) lkv.key_ptr.*[joined_arity..] else rkv.key_ptr.*[joined_arity..], {});
                        }
                    }
                }
                return value.Set{
                    .Finite = .{
                        .arity = arity,
                        .set = set,
                    },
                };
            },
            .Box => |box| {
                var scope = try self.arena.allocator.alloc(value.Scalar, box.scope.len);
                for (box.scope) |name_ix, i| {
                    scope[i] = self.scope.items[self.scope.items.len - 1 - name_ix];
                }
                const box_key = value.LazySet{
                    .expr = box.body,
                    .scope = scope,
                    .time = try self.dupeTime(self.time.items),
                };
                const box_value = try self.interpret(box.body, hint);
                _ = try self.boxes.put(box_key, box_value);
                const tuple = try self.dupeScalars(&[1]value.Scalar{.{ .Box = box_key }});
                var set = DeepHashSet(value.Tuple).init(&self.arena.allocator);
                _ = try set.put(tuple, {});
                return value.Set{
                    .Finite = .{
                        .arity = 1,
                        .set = set,
                    },
                };
            },
            .Fix => |fix| {
                try self.time.append(0);
                const init_key = value.LazySet{
                    .expr = expr,
                    .scope = try self.dupeScalars(self.scope.items),
                    .time = try self.dupeTime(self.time.items),
                };
                const init_set = try self.interpret(fix.init, &[0]value.Scalar{});
                _ = self.time.pop();
                if (init_set != .Finite) {
                    return self.setError(expr, "The initial value for fix must be finite, found {}", .{init_set});
                }

                var fix_hint = try self.arena.allocator.alloc(value.Scalar, 1);

                var fix_key = init_key;
                var fix_set = init_set;
                var iteration: value.Time = 1;
                while (true) : (iteration += 1) {
                    try self.interrupter.check();
                    try self.time.append(iteration);
                    defer _ = self.time.pop();
                    _ = try self.boxes.put(fix_key, fix_set);
                    fix_hint[0] = .{ .Box = fix_key };
                    const body_set = try self.interpret(fix.next, fix_hint);
                    if (body_set != .Finite) {
                        return self.setError(expr, "The body for fix must be finite, found {}", .{body_set});
                    }
                    if (body_set.Finite.arity < 1 and body_set.Finite.set.count() > 0) {
                        return self.setError(expr, "The body for fix must not have arity >= 1", .{});
                    }
                    var new_fix_set = value.Set{
                        .Finite = .{
                            .arity = if (body_set.Finite.arity == 0) 0 else body_set.Finite.arity - 1,
                            .set = DeepHashSet(value.Tuple).init(&self.arena.allocator),
                        },
                    };
                    var body_iter = body_set.Finite.set.iterator();
                    while (body_iter.next()) |kv| {
                        try self.interrupter.check();
                        if (meta.deepEqual(kv.key_ptr.*[0], value.Scalar{ .Box = fix_key })) {
                            _ = try new_fix_set.Finite.set.put(kv.key_ptr.*[1..], {});
                        }
                    }
                    if (meta.deepEqual(fix_set, new_fix_set)) {
                        return fix_set;
                    }
                    fix_key = value.LazySet{
                        .expr = expr,
                        .scope = try self.dupeScalars(self.scope.items),
                        .time = try self.dupeTime(self.time.items),
                    };
                    fix_set = new_fix_set;
                }
            },
            .Reduce => |reduce| {
                const input_set = try self.interpret(reduce.input, &[0]value.Scalar{});
                if (input_set != .Finite) {
                    return self.setError(expr, "The input for reduce must be finite, found {}", .{input_set});
                }
                var input_tuples = try ArrayList(value.Tuple).initCapacity(&self.arena.allocator, input_set.Finite.set.count());
                var input_iter = input_set.Finite.set.iterator();
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
                const init_key = value.LazySet{
                    .expr = expr,
                    .scope = try self.dupeScalars(self.scope.items),
                    .time = try self.dupeTime(self.time.items),
                };
                const init_set = try self.interpret(reduce.init, &[0]value.Scalar{});
                _ = self.time.pop();
                if (init_set != .Finite) {
                    return self.setError(expr, "The initial value for reduce must be finite, found {}", .{init_set});
                }

                var reduce_hint = try self.arena.allocator.alloc(value.Scalar, 2);

                var reduce_key = init_key;
                var reduce_set = init_set;
                for (input_tuples.items) |input_tuple, iteration| {
                    try self.interrupter.check();
                    try self.time.append(iteration);
                    defer _ = self.time.pop();
                    _ = try self.boxes.put(reduce_key, reduce_set);
                    var tuple_set = value.Set{
                        .Finite = .{
                            .arity = input_tuple.len,
                            .set = DeepHashSet(value.Tuple).init(&self.arena.allocator),
                        },
                    };
                    _ = try tuple_set.Finite.set.put(input_tuple, {});
                    const tuple_key = value.LazySet{
                        .expr = reduce.input,
                        .scope = try self.dupeScalars(self.scope.items),
                        .time = try self.dupeTime(self.time.items),
                    };
                    _ = try self.boxes.put(tuple_key, tuple_set);
                    reduce_hint[0] = .{ .Box = reduce_key };
                    reduce_hint[1] = .{ .Box = tuple_key };
                    const body_set = try self.interpret(reduce.next, reduce_hint);
                    if (body_set != .Finite) {
                        return self.setError(expr, "The body for reduce must be finite, found {}", .{body_set});
                    }
                    if (body_set.Finite.arity < 2 and body_set.Finite.set.count() > 0) {
                        return self.setError(expr, "The body for reduce must arity >= 2", .{});
                    }
                    var new_reduce_set = value.Set{
                        .Finite = .{
                            .arity = if (body_set.Finite.arity == 0) 0 else body_set.Finite.arity - 2,
                            .set = DeepHashSet(value.Tuple).init(&self.arena.allocator),
                        },
                    };
                    var body_iter = body_set.Finite.set.iterator();
                    while (body_iter.next()) |kv| {
                        try self.interrupter.check();
                        if (meta.deepEqual(kv.key_ptr.*[0], value.Scalar{ .Box = reduce_key }) and meta.deepEqual(kv.key_ptr.*[1], value.Scalar{ .Box = tuple_key })) {
                            _ = try new_reduce_set.Finite.set.put(kv.key_ptr.*[2..], {});
                        }
                    }
                    reduce_key = value.LazySet{
                        .expr = expr,
                        .scope = try self.dupeScalars(self.scope.items),
                        .time = try self.dupeTime(self.time.items),
                    };
                    reduce_set = new_reduce_set;
                }
                return reduce_set;
            },
            .Enumerate => |body| {
                const body_set = try self.interpret(body, &[0]value.Scalar{});
                if (body_set != .Finite) {
                    return self.setError(expr, "The body for `enumerate` must be finite, found {}", .{body_set});
                }
                var tuples = try ArrayList(value.Tuple).initCapacity(&self.arena.allocator, body_set.Finite.set.count());
                var body_iter = body_set.Finite.set.iterator();
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
                    .Finite = .{
                        .arity = body_set.Finite.arity + 1,
                        .set = set,
                    },
                };
            },
            .Annotate => |annotate| {
                return self.interpret(annotate.body, hint);
            },
            .Native => |native| {
                switch (native) {
                    .Add, .Subtract, .Multiply, .Divide, .Modulus => {
                        if (hint.len < 2) {
                            return value.Set{
                                .Lazy = .{
                                    .expr = expr,
                                    .scope = try self.dupeScalars(self.scope.items),
                                    .time = try self.dupeTime(self.time.items),
                                },
                            };
                        }
                        if (hint[0] != .Number or hint[1] != .Number) {
                            return self.setNativeError(expr, "Inputs to `{s}` must be numbers, found `{} {s} {}`", .{ native.toName(), hint[0], native.toName(), hint[1] });
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
                        const tuple = try self.dupeScalars(&[3]value.Scalar{ hint[0], hint[1], .{ .Number = result } });
                        var set = DeepHashSet(value.Tuple).init(&self.arena.allocator);
                        _ = try set.put(tuple, {});
                        return value.Set{
                            .Finite = .{
                                .arity = 3,
                                .set = set,
                            },
                        };
                    },
                    .Range => {
                        if (hint.len < 2) {
                            return value.Set{
                                .Lazy = .{
                                    .expr = expr,
                                    .scope = try self.dupeScalars(self.scope.items),
                                    .time = try self.dupeTime(self.time.items),
                                },
                            };
                        }
                        if (hint[0] != .Number or hint[1] != .Number) {
                            return self.setNativeError(expr, "Inputs to `range` must be numbers, found `range {} {}`", .{ hint[0], hint[1] });
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
                            const tuple = try self.dupeScalars(&[3]value.Scalar{ hint[0], hint[1], .{ .Number = @intToFloat(f64, i) } });
                            _ = try set.put(tuple, {});
                        }
                        return value.Set{
                            .Finite = .{
                                .arity = 3,
                                .set = set,
                            },
                        };
                    },
                    .GreaterThan, .GreaterThanOrEqual => {
                        if (hint.len < 2) {
                            return value.Set{
                                .Lazy = .{
                                    .expr = expr,
                                    .scope = try self.dupeScalars(self.scope.items),
                                    .time = try self.dupeTime(self.time.items),
                                },
                            };
                        }
                        if (hint[0] != .Number or hint[1] != .Number) {
                            return self.setNativeError(expr, "Inputs to `{s}` must be numbers, found `{} > {}`", .{ native.toName(), hint[0], hint[1] });
                        }
                        var set = DeepHashSet(value.Tuple).init(&self.arena.allocator);
                        const satisfied = switch (native) {
                            .GreaterThan => hint[0].Number > hint[1].Number,
                            .GreaterThanOrEqual => hint[0].Number >= hint[1].Number,
                            else => unreachable,
                        };
                        if (satisfied)
                            _ = try set.put(&[_]value.Scalar{ hint[0], hint[1] }, {});
                        return value.Set{
                            .Finite = .{
                                .arity = 2,
                                .set = set,
                            },
                        };
                    },
                }
            },
        }
    }

    fn dupeScalars(self: *Interpreter, tuple: []const value.Scalar) ![]const value.Scalar {
        return std.mem.dupe(&self.arena.allocator, value.Scalar, tuple);
    }

    fn dupeTime(self: *Interpreter, time: []const value.Time) ![]const value.Time {
        return std.mem.dupe(&self.arena.allocator, value.Time, time);
    }
};
