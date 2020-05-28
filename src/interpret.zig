usingnamespace @import("./common.zig");

const core = @import("./core.zig");
const value = @import("./value.zig");
const Store = @import("./store.zig").Store;

pub fn interpret(store: *const Store, arena: *ArenaAllocator, expr: *const core.Expr, error_info: *?ErrorInfo) Error ! value.Set {
    var interpreter = Interpreter{
        .store = store,
        .arena = arena,
        .scope = ArrayList(value.Scalar).init(&store.arena.allocator),
        .boxes = DeepHashMap(value.Box, value.Set).init(&store.arena.allocator),
        .error_info = error_info,
    };
    return interpreter.interpret(expr);
}

pub const Error = error {
    // sets error_info
    InterpretError,

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
    boxes: DeepHashMap(value.Box, value.Set),
    error_info: *?ErrorInfo,

    fn setError(self: *Interpreter, expr: *const core.Expr, comptime fmt: []const u8, args: var) Error {
        const message = try format(&self.arena.allocator, fmt, args);
        self.error_info.* = ErrorInfo{
            .expr = expr,
            .message = message,
        };
        return error.InterpretError;
    }

    fn dupeTuple(self: *Interpreter, tuple: []const value.Scalar) ! []const value.Scalar {
        return std.mem.dupe(&self.arena.allocator, value.Scalar, tuple);
    }

    fn interpret(self: *Interpreter, expr: *const core.Expr) Error ! value.Set {
        switch (expr.*) {
            .None =>  {
                const set = value.FiniteSet.init(&self.arena.allocator);
                return value.Set{.Finite = set};
            },
            .Some => {
                var set = value.FiniteSet.init(&self.arena.allocator);
                _ = try set.put(&[0]value.Scalar{}, {});
                return value.Set{.Finite = set};
            },
            .Scalar => |scalar| {
                var set = value.FiniteSet.init(&self.arena.allocator);
                _ = try set.put(try self.dupeTuple(&[1]value.Scalar{scalar}), {});
                return value.Set{.Finite = set};
            },
            .Union => |pair| {
                const left = try self.interpret(pair.left);
                const right = try self.interpret(pair.right);
                if (left == .Finite and right == .Finite) {
                    var set = value.FiniteSet.init(&self.arena.allocator);
                    var leftIter = left.Finite.iterator();
                    while (leftIter.next()) |kv| {
                        _ = try set.put(kv.key, {});
                    }
                    var rightIter = right.Finite.iterator();
                    while (rightIter.next()) |kv| {
                        _ = try set.put(kv.key, {});
                    }
                    return value.Set{.Finite = set};
                } else {
                    return self.setError(expr, "Cannot union two lazy sets", .{});
                }
            },
            .Intersect => |pair| {
                const left = try self.interpret(pair.left);
                const right = try self.interpret(pair.right);
                if (left == .Finite and right == .Finite) {
                    var set = value.FiniteSet.init(&self.arena.allocator);
                    var leftIter = left.Finite.iterator();
                    while (leftIter.next()) |kv| {
                        if (right.Finite.contains(kv.key)) {
                            _ = try set.put(kv.key, {});
                        }
                    }
                    return value.Set{.Finite = set};
                } else {
                    return self.setError(expr, "Cannot intersect two lazy sets", .{});
                }
            },
            .Product => |pair| {
                const left = try self.interpret(pair.left);
                const right = try self.interpret(pair.right);
                if (left == .Finite and right == .Finite) {
                    var set = value.FiniteSet.init(&self.arena.allocator);
                    var leftIter = left.Finite.iterator();
                    while (leftIter.next()) |lkv| {
                        var rightIter = right.Finite.iterator();
                        while (rightIter.next()) |rkv| {
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
                    return value.Set{.Finite = set};
                } else {
                    return self.setError(expr, "Cannot product two lazy sets", .{});
                }
            },
            .Equal => |pair| {
                const left = try self.interpret(pair.left);
                const right = try self.interpret(pair.right);
                if (left == .Finite and right == .Finite) {
                    var set = value.FiniteSet.init(&self.arena.allocator);
                    const isEqual = isEqual: {
                        var leftIter = left.Finite.iterator();
                        while (leftIter.next()) |kv| {
                            if (!right.Finite.contains(kv.key)) {
                                break :isEqual false;
                            }
                        }
                        var rightIter = right.Finite.iterator();
                        while (rightIter.next()) |kv| {
                            if (!left.Finite.contains(kv.key)) {
                                break :isEqual false;
                            }
                        }
                        break :isEqual true;
                    };
                    if (isEqual) {
                        _ = try set.put(&[0]value.Scalar{}, {});
                    }
                    return value.Set{.Finite = set};
                } else {
                    return self.setError(expr, "Cannot equal two lazy sets", .{});
                }
            },
            .Name => |name_ix| {
                var set = value.FiniteSet.init(&self.arena.allocator);
                const scalar = self.scope.items[self.scope.items.len - 1 - name_ix];
                _ = try set.put(try self.dupeTuple(&[1]value.Scalar{scalar}), {});
                return value.Set{.Finite = set};
            },
            .UnboxName => |name_ix| {
                const scalar = self.scope.items[self.scope.items.len - 1 - name_ix];
                switch (scalar) {
                    .Box => |box| {
                        if (self.boxes.getValue(box)) |set| {
                            return set;
                        } else {
                            panic("No box for {}", .{box});
                        }
                    },
                    else => {
                        return self.setError(expr, "Tried to unbox {} which is not a box", .{scalar});
                    }
                }
            },
            .When => |when| {
                const condition = try self.interpret(when.condition);
                if (condition == .Lazy) {
                    return self.setError(expr, "The condition for `when` cannot be a lazy set", .{});
                }
                if (condition.Finite.count() == 0) {
                    const set = value.FiniteSet.init(&self.arena.allocator);
                    return value.Set{.Finite = set};
                } else {
                    return self.interpret(when.true_branch);
                }
            },
            .Abstract => |body| {
                const scope = try std.mem.dupe(&self.arena.allocator, value.Scalar, self.scope.items);
                return value.Set{.Lazy = .{
                    .scope = scope,
                    .expr = expr,
                }};
            },
            .Apply => |pair| {
                var left = try self.interpret(pair.left);
                var right = try self.interpret(pair.right);
                return self.apply(expr, left, right);
            },
            .Box => |box| {
                var scope = try self.arena.allocator.alloc(value.Scalar, box.scope.len);
                for (box.scope) |name_ix, i| {
                    scope[i] = self.scope.items[self.scope.items.len - 1 - name_ix];
                }
                const box_key = value.Box{
                    .expr = box.body,
                    .scope = scope,
                };
                const box_value = try self.interpret(box.body);
                _ = try self.boxes.put(box_key, box_value);
                const tuple = try self.dupeTuple(&[1]value.Scalar{.{.Box = box_key}});
                var set = value.FiniteSet.init(&self.arena.allocator);
                _ = try set.put(tuple, {});
                return value.Set{.Finite = set};
            },
            .Annotate => |annotate| {
                return self.interpret(annotate.body);
            },
            .Native => |native| {
                TODO();
            },
        }
    }

    fn apply(self: *Interpreter, expr: *const core.Expr, left_: value.Set, right_: value.Set) Error ! value.Set {
        var left = left_;
        var right = right_;
        if (left == .Lazy and right == .Lazy) {
            return self.setError(expr, "Cannot apply a lazy set to a lazy set", .{});
        } else {
            if (right == .Lazy) {
                std.mem.swap(value.Set, &left, &right);
            }
            assert(right == .Finite);
            var set = value.FiniteSet.init(&self.arena.allocator);
            var right_iter = right.Finite.iterator();
            while (right_iter.next()) |rkv| {
                var result = left;
                for (rkv.key) |arg, i| {
                    result = try self.apply1(expr, result, arg);
                }
                if (result == .Lazy) {
                    // don't have enough values to force this yet
                    const scope = try std.mem.dupe(&self.arena.allocator, value.Scalar, self.scope.items);
                    return value.Set{.Lazy = .{
                        .scope = scope,
                        .expr = expr,
                    }};
                }
                var result_iter = result.Finite.iterator();
                while (result_iter.next()) |kv| {
                    _ = try set.put(kv.key, {});
                }
            }
            return value.Set{.Finite = set};
        }
    }

    fn apply1(self: *Interpreter, expr: *const core.Expr, fun: value.Set, arg: value.Scalar) Error ! value.Set {
        switch (fun) {
            .Finite => |finite| {
                var set = value.FiniteSet.init(&self.arena.allocator);
                var iter = finite.iterator();
                while (iter.next()) |kv| {
                    if (kv.key.len == 0) {
                        _ = try set.put(try self.dupeTuple(&[1]value.Scalar{arg}), {});
                    } else {
                        if (meta.deepEqual(kv.key[0], arg)) {
                            _ = try set.put(kv.key[1..], {});
                        }
                    }
                }
                return value.Set{.Finite = set};
            },
            .Lazy => |lazy| {
                const old_scope = self.scope;
                defer self.scope = old_scope;
                self.scope = try ArrayList(value.Scalar).initCapacity(&self.arena.allocator, lazy.scope.len);
                try self.scope.appendSlice(lazy.scope);
                switch (lazy.expr.*) {
                    .Abstract => |body| {
                        try self.scope.append(arg);
                        return self.interpret(body);
                    },
                    .Apply => |pair| {
                        var left = try self.interpret(pair.left);
                        var right = try self.interpret(pair.right);
                        if (right == .Lazy) {
                            std.mem.swap(value.Set, &left, &right);
                        }
                        assert(right == .Finite);
                        var new_right = value.FiniteSet.init(&self.arena.allocator);
                        var right_iter = right.Finite.iterator();
                        while (right_iter.next()) |kv| {
                            const tuple = kv.key;
                            var new_tuple = try ArrayList(value.Scalar).initCapacity(&self.arena.allocator, tuple.len + 1);
                            try new_tuple.appendSlice(tuple);
                            try new_tuple.append(arg);
                            _ = try new_right.put(try self.dupeTuple(new_tuple.items), {});
                        }
                        return self.apply(expr, left, .{.Finite = new_right});
                    },
                    else => panic("What are this? {}", .{lazy.expr.*}),
                }
            },
        }
    }
};

const parse = if (builtin.is_test) @import("./parse.zig");
const desugar = if (builtin.is_test) @import("./desugar.zig");

fn testInterpret(source: []const u8, expected: []const u8) !void {
    warn("Source:\n{}\n", .{source});
    var arena = ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var store = Store.init(&arena);
    var parse_error_info: ?parse.ErrorInfo = null;
    const syntax_expr = try parse.parse(&store, source, &parse_error_info);
    var desugar_error_info: ?desugar.ErrorInfo = null;
    const core_expr = try desugar.desugar(&store, syntax_expr, &desugar_error_info);
    var error_info: ?ErrorInfo = null;
    if (interpret(&store, &arena, core_expr, &error_info)) |found| {
        var bytes = ArrayList(u8).init(std.testing.allocator);
        defer bytes.deinit();
        try found.dumpInto(std.testing.allocator, bytes.outStream());
        if (!meta.deepEqual(expected, bytes.items)) {
            panic("\nExpected interpret:\n{}\n\nFound interpret:\n{}", .{expected, bytes.items});
        }
    } else |err| {
        warn("\nExpected interpret:\n{}\n\nFound error:\n{}\n", .{expected, error_info.?.message});
        return err;
    }
}

fn testInterpretError(source: []const u8, expected: []const u8) !void {
    warn("Source:\n{}\n", .{source});
    var arena = ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var store = Store.init(&arena);
    var parse_error_info: ?parse.ErrorInfo = null;
    const syntax_expr = try parse.parse(&store, source, &parse_error_info);
    var desugar_error_info: ?desugar.ErrorInfo = null;
    const core_expr = try desugar.desugar(&store, syntax_expr, &desugar_error_info);
    var error_info: ?ErrorInfo = null;
    if (interpret(&store, &arena, core_expr, &error_info)) |found| {
        var bytes = ArrayList(u8).init(std.testing.allocator);
        try found.dumpInto(std.testing.allocator, bytes.outStream());
        panic("\nExpected error:\n{}\n\nFound interpret:\n{}", .{expected, bytes.items});
    } else |err| {
        if (!meta.deepEqual(expected, error_info.?.message)) {
            warn("\nExpected error:\n{}\n\nFound error:\n{}\n", .{expected, error_info.?.message});
            return err;
        }
    }
}

test "interpret" {
    try testInterpret(
        \\1
            ,
        \\1
    );

    try testInterpret(
        \\1 . "foo"
            ,
        \\1 . "foo"
    );

    try testInterpret(
        \\1 . "foo" | 2 . "bar"
            ,
        \\2 . "bar" | 1 . "foo"
    );

    try testInterpret(
        \\1 . ("foo" | 2) . "bar"
            ,
        \\1 . "foo" . "bar" | 1 . 2 . "bar"
    );

    try testInterpret(
        \\!(1 . "foo")
            ,
        \\none
    );

    try testInterpret(
        \\when some then 1 . 2
            ,
        \\1 . 2
    );

    try testInterpret(
        \\when none then 1 . 2
            ,
        \\none
    );

    try testInterpret(
        \\let a = 1 . "foo" in
        \\a . a
            ,
        \\1 . "foo" . 1 . "foo"
    );

    try testInterpret(
        \\let foo = \ a b -> a = b in
        \\foo 1
            ,
        \\(lazy #10;)
    );

    try testInterpret(
        \\let foo = \ a b -> a = b in
        \\foo 1 1
            ,
        \\some
    );

    try testInterpret(
        \\let foo = \ a b -> a = b in
        \\foo 1 2
            ,
        \\none
    );

    try testInterpret(
        \\let a = [1 . "foo"] in
        \\a \ [a] -> a . a
            ,
        \\1 . "foo" . 1 . "foo"
    );

    try testInterpret(
        \\let foo = \ a -> [\ b -> a . b] in
        \\foo 1
            ,
        \\[box #3; 1]
    );

    // TODO this is a problem
    try testInterpret(
        \\let foo = \ a -> [\ b -> a . b] in
        \\(foo 1) | (foo 2)
            ,
        \\[box #3; 2] | [box #3; 1]
    );

    try testInterpret(
        \\let foo = \ a -> [\ b -> a . b] in
        \\(1 . (foo 1)) | (2 . (foo 2))
            ,
        \\1 . [box #3; 1] | 2 . [box #3; 2]
    );

    try testInterpret(
        \\let foo = \ a -> [\ b -> a . b] in
        \\let bar = (1 . (foo 1)) | (2 . (foo 2)) in
        \\bar \ a [f] -> a . (f 41)
            ,
        \\2 . 2 . 41 | 1 . 1 . 41
    );

    try testInterpretError(
        \\let foo1 = \ a -> [\ b -> a . b] in
        \\let foo2 = \ a -> [\ b -> a . b] in
        \\let bar = (1 . (foo1 1)) | (2 . (foo2 2)) in
        \\bar \ a [f] -> a . (f 41)
            ,
        \\Don't know what type will result from unboxing type ScalarType{ .Any = void }
    );

    try testInterpret(
        \\let foo = 1 . 1 | 2 . 2 in
        \\foo foo
            ,
        ""
    );

    try testInterpretError(
        \\let foo = \ a b -> a = b in
        \\foo foo
            ,
        \\Cannot apply two maybe-infinite sets
    );

    try testInterpretError(
        \\let foo = \ a b -> a = b in
        \\foo = foo
            ,
        \\Cannot = two maybe-infinite sets
    );

    try testInterpretError(
        \\let foo = \ a b -> a = b in
        \\foo | 1 . 2
            ,
        \\TODO cannot union two maybe-infinite sets
    );
}
