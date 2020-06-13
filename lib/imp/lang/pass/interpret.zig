const imp = @import("../../../imp.zig");
usingnamespace imp.common;
const meta = imp.meta;
const Store = imp.lang.Store;
const core = imp.lang.repr.core;
const value = imp.lang.repr.value;

/// Guarantees:
/// * If this returns a value.Set.Finite then it will be the correct answer
/// * If expr typechecks then this will not return InterpretError
/// * If expr has a finite type then this will return a value.Set.Finite
pub fn interpret(store: *const Store, arena: *ArenaAllocator, expr: *const core.Expr, error_info: *?ErrorInfo) Error ! value.Set {
    var interpreter = Interpreter{
        .store = store,
        .arena = arena,
        .scope = ArrayList(value.Scalar).init(&store.arena.allocator),
        .time = ArrayList(value.Time).init(&store.arena.allocator),
        .boxes = DeepHashMap(value.LazySet, value.Set).init(&store.arena.allocator),
        .error_info = error_info,
    };
    return interpreter.interpret(expr, &[0]value.Scalar{});
}

pub const Error = error {
    // sets error_info
    InterpretError,
    NativeError,

    // does not set error_info
    OutOfMemory
};

pub const ErrorInfo = struct {
    expr: *const core.Expr,
    message: []const u8,
};

// --------------------------------------------------------------------------------

const Interpreter = struct {
    store: *const Store,
    arena: *ArenaAllocator,
    scope: ArrayList(value.Scalar),
    time: ArrayList(value.Time),
    boxes: DeepHashMap(value.LazySet, value.Set),
    error_info: *?ErrorInfo,

    fn setError(self: *Interpreter, expr: *const core.Expr, comptime fmt: []const u8, args: var) Error {
        const message = try format(&self.arena.allocator, fmt, args);
        self.error_info.* = ErrorInfo{
            .expr = expr,
            .message = message,
        };
        return error.InterpretError;
    }

    fn setNativeError(self: *Interpreter, expr: *const core.Expr, comptime fmt: []const u8, args: var) Error {
        const message = try format(&self.arena.allocator, fmt, args);
        self.error_info.* = ErrorInfo{
            .expr = expr,
            .message = message,
        };
        return error.NativeError;
    }

    fn interpret(self: *Interpreter, expr: *const core.Expr, hint: value.Tuple) Error ! value.Set {
        switch (expr.*) {
            .None =>  {
                const set = DeepHashSet(value.Tuple).init(&self.arena.allocator);
                return value.Set{.Finite = .{
                    .arity = 0,
                    .set = set,
                }};
            },
            .Some => {
                var set = DeepHashSet(value.Tuple).init(&self.arena.allocator);
                _ = try set.put(&[0]value.Scalar{}, {});
                return value.Set{.Finite = .{
                    .arity = 0,
                    .set = set,
                }};
            },
            .Scalar => |scalar| {
                var set = DeepHashSet(value.Tuple).init(&self.arena.allocator);
                _ = try set.put(try self.dupeScalars(&[1]value.Scalar{scalar}), {});
                return value.Set{.Finite = .{
                    .arity = 1,
                    .set = set
                }};
            },
            .Union => |pair| {
                const left = try self.interpret(pair.left, hint);
                const right = try self.interpret(pair.right, hint);
                if (left == .Lazy or right == .Lazy) {
                    return value.Set{.Lazy = .{
                        .expr = expr,
                        .scope = try self.dupeScalars(self.scope.items),
                        .time = try self.dupeTime(self.time.items),
                    }};
                }
                if (left.Finite.arity != right.Finite.arity and left.Finite.set.count() > 0 and right.Finite.set.count() > 0) {
                    return self.setError(expr, "Tried to union sets with different arities: {} vs {}", .{left.Finite.arity, right.Finite.arity});
                }
                var set = DeepHashSet(value.Tuple).init(&self.arena.allocator);
                var left_iter = left.Finite.set.iterator();
                while (left_iter.next()) |kv| {
                    _ = try set.put(kv.key, {});
                }
                var right_iter = right.Finite.set.iterator();
                while (right_iter.next()) |kv| {
                    _ = try set.put(kv.key, {});
                }
                return value.Set{.Finite = .{
                    .arity = max(left.Finite.arity, right.Finite.arity),
                    .set = set,
                }};
            },
            .Intersect => |pair| {
                const left = try self.interpret(pair.left, hint);
                const right = try self.interpret(pair.right, hint);
                if (left == .Lazy or right == .Lazy) {
                    return value.Set{.Lazy = .{
                        .expr = expr,
                        .scope = try self.dupeScalars(self.scope.items),
                        .time = try self.dupeTime(self.time.items),
                    }};
                }
                if (left.Finite.arity != right.Finite.arity and left.Finite.set.count() > 0 and right.Finite.set.count() > 0) {
                    return self.setError(expr, "Tried to intersect sets with different arities: {} vs {}", .{left.Finite.arity, right.Finite.arity});
                }
                var set = DeepHashSet(value.Tuple).init(&self.arena.allocator);
                var left_iter = left.Finite.set.iterator();
                while (left_iter.next()) |kv| {
                    if (right.Finite.set.contains(kv.key)) {
                        _ = try set.put(kv.key, {});
                    }
                }
                return value.Set{.Finite = .{
                    .arity = max(left.Finite.arity, right.Finite.arity),
                    .set = set,
                }};
            },
            .Product => |pair| {
                const left = try self.interpret(pair.left, hint);
                if (left == .Lazy) {
                    return value.Set{.Lazy = .{
                        .expr = expr,
                        .scope = try self.dupeScalars(self.scope.items),
                        .time = try self.dupeTime(self.time.items),
                    }};
                }
                const right = try self.interpret(pair.right, hint[min(hint.len, left.Finite.arity)..]);
                if (right == .Lazy) {
                    return value.Set{.Lazy = .{
                        .expr = expr,
                        .scope = try self.dupeScalars(self.scope.items),
                        .time = try self.dupeTime(self.time.items),
                    }};
                }
                var set = DeepHashSet(value.Tuple).init(&self.arena.allocator);
                var left_iter = left.Finite.set.iterator();
                while (left_iter.next()) |lkv| {
                    var right_iter = right.Finite.set.iterator();
                    while (right_iter.next()) |rkv| {
                        var tuple = try self.arena.allocator.alloc(value.Scalar, lkv.key.len + rkv.key.len);
                        var i: usize = 0;
                        for (lkv.key) |scalar| {
                            tuple[i] = scalar;
                            i += 1;
                        }
                        for (rkv.key) |scalar| {
                            tuple[i] = scalar;
                            i += 1;
                        }
                        _ = try set.put(tuple, {});
                    }
                }
                return value.Set{.Finite = .{
                    .arity = left.Finite.arity + right.Finite.arity,
                    .set = set,
                }};
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
                        if (!right.Finite.set.contains(kv.key)) {
                            break :isEqual false;
                        }
                    }
                    var rightIter = right.Finite.set.iterator();
                    while (rightIter.next()) |kv| {
                        if (!left.Finite.set.contains(kv.key)) {
                            break :isEqual false;
                        }
                    }
                    break :isEqual true;
                };
                if (isEqual) {
                    _ = try set.put(&[0]value.Scalar{}, {});
                }
                return value.Set{.Finite = .{
                    .arity = 0,
                    .set = set,
                }};
            },
            .Name => |name_ix| {
                var set = DeepHashSet(value.Tuple).init(&self.arena.allocator);
                const scalar = self.scope.items[self.scope.items.len - 1 - name_ix];
                _ = try set.put(try self.dupeScalars(&[1]value.Scalar{scalar}), {});
                return value.Set{.Finite = .{
                    .arity = 1,
                    .set = set,
                }};
            },
            .UnboxName => |name_ix| {
                const scalar = self.scope.items[self.scope.items.len - 1 - name_ix];
                switch (scalar) {
                    .Box => |box| {
                        if (self.boxes.getValue(box)) |set| {
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
                                }
                            }
                        } else {
                            imp_panic("No box for {}", .{box});
                        }
                    },
                    else => {
                        return self.setError(expr, "Tried to unbox {} which is not a box", .{scalar});
                    }
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
                return value.Set{.Finite = .{
                    .arity = 0,
                    .set = set,
                }};
            },
            .When => |when| {
                // the hint for expr doesn't tell us anything about condition
                const condition = try self.interpret(when.condition, &[0]value.Scalar{});
                if (condition == .Lazy) {
                    return self.setError(expr, "The condition for `when` cannot be a lazy set", .{});
                }
                if (condition.Finite.set.count() == 0) {
                    const set = DeepHashSet(value.Tuple).init(&self.arena.allocator);
                    return value.Set{.Finite = .{
                        .arity = 0,
                        .set = set,
                    }};
                } else {
                    return self.interpret(when.true_branch, hint);
                }
            },
            .Abstract => |body| {
                if (hint.len == 0) {
                    return value.Set{.Lazy = .{
                        .expr = expr,
                        .scope = try self.dupeScalars(self.scope.items),
                        .time = try self.dupeTime(self.time.items),
                    }};
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
                                var tuple = try ArrayList(value.Scalar).initCapacity(&self.arena.allocator, kv.key.len + 1);
                                try tuple.append(hint[0]);
                                try tuple.appendSlice(kv.key);
                                _ = try set.put(tuple.items, {});
                            }
                            return value.Set{.Finite =.{
                                .arity = arity,
                                .set = set,
                            }};
                        },
                        .Lazy => |lazy| {
                            // couldn't fully specialize, give up
                            return value.Set{.Lazy = .{
                                .expr = expr,
                                .scope = try self.dupeScalars(self.scope.items),
                                .time = try self.dupeTime(self.time.items),
                            }};
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
                        var left_hint = try ArrayList(value.Scalar).initCapacity(&self.arena.allocator, right.Finite.arity + hint.len);
                        try left_hint.appendSlice(kv.key);
                        try left_hint.appendSlice(hint);
                        const left_part = try self.interpret(pair_left, left_hint.items);
                        if (left_part == .Lazy) {
                            // couldn't fully specialize, give up
                            return value.Set{.Lazy = .{
                                .expr = expr,
                                .scope = try self.dupeScalars(self.scope.items),
                                .time = try self.dupeTime(self.time.items),
                            }};
                        }
                        if (left_arity_o) |left_arity| {
                            if (left_arity != left_part.Finite.arity and left_part.Finite.set.count() > 0) {
                                return self.setError(expr, "Apply resulted in unions over sets of different arities: {} vs {}", .{left_arity, left_part.Finite.arity});
                            }
                        } else {
                            left_arity_o = left_part.Finite.arity;
                        }
                        var left_part_iter = left_part.Finite.set.iterator();
                        while (left_part_iter.next()) |lkv| {
                            _ = try left_set.put(lkv.key, {});
                        }
                    }
                    left = .{.Finite = .{
                    // TODO if left_arity is never set then arity of apply could be wrong - not sure if this matters
                        .arity = left_arity_o orelse 0,
                        .set = left_set,
                    }};
                }
                const joined_arity = min(left.Finite.arity, right.Finite.arity);
                const arity = max(left.Finite.arity, right.Finite.arity) - joined_arity;
                var set = DeepHashSet(value.Tuple).init(&self.arena.allocator);
                var left_iter = left.Finite.set.iterator();
                while (left_iter.next()) |lkv| {
                    var right_iter = right.Finite.set.iterator();
                    while (right_iter.next()) |rkv| {
                        if (meta.deepEqual(lkv.key[0..joined_arity], rkv.key[0..joined_arity])) {
                            _ = try set.put(if (left.Finite.arity > right.Finite.arity) lkv.key[joined_arity..] else rkv.key[joined_arity..], {});
                        }
                    }
                }
                return value.Set{.Finite = .{
                    .arity = arity,
                    .set = set,
                }};
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
                const tuple = try self.dupeScalars(&[1]value.Scalar{.{.Box = box_key}});
                var set = DeepHashSet(value.Tuple).init(&self.arena.allocator);
                _ = try set.put(tuple, {});
                return value.Set{.Finite = .{
                    .arity = 1,
                    .set = set,
                }};
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
                    try self.time.append(iteration);
                    _ = try self.boxes.put(fix_key, fix_set);
                    fix_hint[0] = .{.Box = fix_key};
                    const body_set = try self.interpret(fix.next, fix_hint);
                    if (body_set != .Finite) {
                        return self.setError(expr, "The body for fix must be finite, found {}", .{body_set});
                    }
                    if (body_set.Finite.arity == 0) {
                        return self.setError(expr, "The body for fix must not have arity 0", .{});
                    }
                    var new_fix_set = value.Set{.Finite = .{
                        .arity = body_set.Finite.arity - 1,
                        .set = DeepHashSet(value.Tuple).init(&self.arena.allocator),
                    }};
                    var body_iter = body_set.Finite.set.iterator();
                    while (body_iter.next()) |kv| {
                        if (meta.deepEqual(kv.key[0], value.Scalar{.Box = fix_key})) {
                            _ = try new_fix_set.Finite.set.put(kv.key[1..], {});
                        }
                    }
                    if (meta.deepEqual(fix_set, new_fix_set)) {
                        return fix_set;
                    }
                    const new_fix_key = value.LazySet{
                        .expr = expr,
                        .scope = try self.dupeScalars(self.scope.items),
                        .time = try self.dupeTime(self.time.items),
                    };
                    fix_set = new_fix_set;
                    _ = self.time.pop();
                }
            },
            .Annotate => |annotate| {
                return self.interpret(annotate.body, hint);
            },
            .Native => |native| {
                switch (native) {
                    .Add, .Subtract, .Multiply, .Divide => {
                        if (hint.len < 2) {
                            return value.Set{.Lazy = .{
                                .expr = expr,
                                .scope = try self.dupeScalars(self.scope.items),
                                .time = try self.dupeTime(self.time.items),
                            }};
                        }
                        if (hint[0] != .Number or hint[1] != .Number) {
                            return self.setNativeError(expr, "Inputs to + must be numbers, found {} + {}", .{hint[0], hint[1]});
                        }
                        if (native == .Divide and hint[1].Number == 0) {
                            return self.setNativeError(expr, "Divide by 0", .{});
                        }
                        const result = switch (native) {
                            .Add => hint[0].Number + hint[1].Number,
                            .Subtract => hint[0].Number - hint[1].Number,
                            .Multiply => hint[0].Number * hint[1].Number,
                            .Divide => hint[0].Number / hint[1].Number,
                        };
                        const tuple = try self.dupeScalars(&[3]value.Scalar{hint[0], hint[1], .{.Number = result}});
                        var set = DeepHashSet(value.Tuple).init(&self.arena.allocator);
                        _ = try set.put(tuple, {});
                        return value.Set{.Finite = .{
                            .arity = 3,
                            .set = set,
                        }};
                    },
                }
            },
        }
    }

    fn dupeScalars(self: *Interpreter, tuple: []const value.Scalar) ! []const value.Scalar {
        return std.mem.dupe(&self.arena.allocator, value.Scalar, tuple);
    }

    fn dupeTime(self: *Interpreter, time: []const value.Time) ! []const value.Time {
        return std.mem.dupe(&self.arena.allocator, value.Time, time);
    }
};
