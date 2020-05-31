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

    fn lazyPair(self: *Interpreter, left: value.Set, right: value.Set) Error ! value.LazyPair {
        var left_p = try self.arena.allocator.create(value.Set);
        var right_p = try self.arena.allocator.create(value.Set);
        left_p.* = left;
        right_p.* = right;
        return value.LazyPair{
            .left = left_p,
            .right = right_p,
        };
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
                return self.union_(expr, left, right);
            },
            .Intersect => |pair| {
                const left = try self.interpret(pair.left);
                const right = try self.interpret(pair.right);
                return self.intersect(expr, left, right);
            },
            .Product => |pair| {
                const left = try self.interpret(pair.left);
                const right = try self.interpret(pair.right);
                return self.product(expr, left, right);
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
                            imp_panic("No box for {}", .{box});
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
                return value.Set{.Lazy = .{.Abstract = .{
                    .body = body,
                    .scope = scope,
                }}};
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
            // .Native => |native| {
            //     TODO();
            // },
        }
    }

    fn union_(self: *Interpreter, expr: *const core.Expr, left: value.Set, right: value.Set) Error ! value.Set {
        if (left == .Finite and right == .Finite) {
            var set = value.FiniteSet.init(&self.arena.allocator);
            var left_iter = left.Finite.iterator();
            var left_arity_o: ?usize = null;
            while (left_iter.next()) |kv| {
                left_arity_o = kv.key.len;
                _ = try set.put(kv.key, {});
            }
            var right_iter = right.Finite.iterator();
            var right_arity_o: ?usize = null;
            while (right_iter.next()) |kv| {
                right_arity_o = kv.key.len;
                _ = try set.put(kv.key, {});
            }
            if (left_arity_o) |left_arity| {
                if (right_arity_o) |right_arity| {
                    if (left_arity != right_arity) {
                        return self.setError(expr, "Tried to union sets with different arities: {} vs {}", .{left_arity, right_arity});
                    }
                }
            }
            return value.Set{.Finite = set};
        } else {
            return value.Set{.Lazy = .{.Union = try self.lazyPair(left, right)}};
        }
    }

    fn intersect(self: *Interpreter, expr: *const core.Expr, left: value.Set, right: value.Set) Error ! value.Set {
        if (left == .Finite and right == .Finite) {
            var set = value.FiniteSet.init(&self.arena.allocator);
            var left_iter = left.Finite.iterator();
            while (left_iter.next()) |kv| {
                if (right.Finite.contains(kv.key)) {
                    _ = try set.put(kv.key, {});
                }
            }
            return value.Set{.Finite = set};
        } else {
            return value.Set{.Lazy = .{.Intersect = try self.lazyPair(left, right)}};
        }
    }

    fn product(self: *Interpreter, expr: *const core.Expr, left: value.Set, right: value.Set) Error ! value.Set {
        if (left == .Finite and right == .Finite) {
            var set = value.FiniteSet.init(&self.arena.allocator);
            var left_iter = left.Finite.iterator();
            while (left_iter.next()) |lkv| {
                var right_iter = right.Finite.iterator();
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
            return value.Set{.Finite = set};
        } else {
            return value.Set{.Lazy = .{.Product = try self.lazyPair(left, right)}};
        }
    }

    fn apply(self: *Interpreter, expr: *const core.Expr, left_: value.Set, right_: value.Set) Error ! value.Set {
        var left = left_;
        var right = right_;
        if (left == .Lazy and right == .Lazy) {
            return self.setError(expr, "Cannot apply two lazy sets", .{});
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
                    // don't have enough values to fully force this yet
                    return value.Set{.Lazy = .{.Apply = try self.lazyPair(left, right)}};
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
                switch (lazy) {
                    // `(a -> body) arg` => `let a = arg in body`
                    .Abstract => |abstract| {
                        const old_scope = self.scope;
                        defer self.scope = old_scope;
                        self.scope = try ArrayList(value.Scalar).initCapacity(&self.arena.allocator, abstract.scope.len);
                        try self.scope.appendSlice(abstract.scope);
                        try self.scope.append(arg);
                        return self.interpret(abstract.body);
                    },
                    // assume left is the lazy set
                    // we know arity(left) > arity(right) or this would have been forced already
                    // `(left right) arg` => `left (right . arg)`
                    .Apply => |pair| {
                        const left = pair.left.*;
                        const right = pair.right.*;
                        assert(left == .Lazy and right == .Finite);
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
                    // `(left | right) arg` => `(left arg | right arg)`
                    .Union => |pair| {
                        const new_left = try self.apply1(expr, pair.left.*, arg);
                        const new_right = try self.apply1(expr, pair.right.*, arg);
                        return self.union_(expr, new_left, new_right);
                    },
                    // `(left & right) arg` => `(left arg & right arg)`
                    .Intersect => |pair| {
                        const new_left = try self.apply1(expr, pair.left.*, arg);
                        const new_right = try self.apply1(expr, pair.right.*, arg);
                        return self.intersect(expr, new_left, new_right);
                    },
                    .Product => |pair| {
                        if (pair.left.* == .Finite) {
                            const left = pair.left.*.Finite;
                            // `(none . right) arg` => `none`
                            if (left.count() == 0) {
                                return value.Set{.Finite = value.FiniteSet.init(&self.arena.allocator)};
                            }
                            // `(some . right) arg` => `right arg`
                            if (left.count() == 1 and left.iterator().next().?.key.len == 0) {
                                return self.apply1(expr, pair.right.*, arg);
                            }
                        }
                        // in all other cases
                        // `(left . right) arg` => `(left arg) . right`
                        // (if left is `none`, the answer is none anyway)
                        // (left cannot be `some` because the arity of a lazy set is always >= 1)
                        const new_left = try self.apply1(expr, pair.left.*, arg);
                        return self.product(expr, new_left, pair.right.*);
                    },
                }
            },
        }
    }
};

const parse = if (builtin.is_test) @import("./parse.zig");
const desugar = if (builtin.is_test) @import("./desugar.zig");

fn testInterpret(source: []const u8, expected: []const u8) !void {
    errdefer warn("\nSource:\n{}\n", .{source});
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
            warn("\nExpected interpret:\n{}\n\nFound interpret:\n{}\n", .{expected, bytes.items});
            return error.TestFailure;
        }
    } else |err| {
        warn("\nExpected interpret:\n{}\n\nFound error:\n{}\n", .{expected, if (err == error.InterpretError) error_info.?.message else ""});
        return err;
    }
}

fn testInterpretError(source: []const u8, expected: []const u8) !void {
    errdefer warn("\nSource:\n{}\n", .{source});
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
        warn("\nExpected error:\n{}\n\nFound interpret:\n{}\n", .{expected, bytes.items});
        return error.TestFailure;
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
        \\1 . "foo" | 2 . "bar"
    );

    try testInterpret(
        \\2 . "bar" | 1 . "foo"
            ,
        \\1 . "foo" | 2 . "bar"
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
        \\(lazy)
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
        \\[box #3; 1] | [box #3; 2]
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
        \\1 . 1 . 41 | 2 . 2 . 41
    );

    try testInterpret(
        \\let foo1 = \ a -> [\ b -> a . b] in
        \\let foo2 = \ a -> [\ b -> a . b] in
        \\let bar = (1 . (foo1 1)) | (2 . (foo2 2)) in
        \\bar \ a [f] -> a . (f 41)
            ,
        \\1 . 1 . 41 | 2 . 2 . 41
    );

    try testInterpret(
        \\let foo = 1 . 1 | 2 . 2 in
        \\foo foo
            ,
        \\some
    );

    try testInterpretError(
        \\let foo = \ a b -> a = b in
        \\foo foo
            ,
        \\Cannot apply two lazy sets
    );

    try testInterpretError(
        \\let foo = \ a b -> a = b in
        \\foo = foo
            ,
        \\Cannot equal two lazy sets
    );

    try testInterpret(
        \\let foo = \ a b -> a = b in
        \\foo | 1 . 2
            ,
        \\(lazy)
    );

    try testInterpret(
        \\let foo = \ a b -> a = b in
        \\(foo | 1 . 2) 1 2
            ,
        \\some
    );

    try testInterpret(
        \\let foo = \ a b -> a = b in
        \\(foo & (1 . 2)) 1 2
            ,
        \\none
    );

    try testInterpret(
        \\let foo = \ a b -> a = b in
        \\(foo . 1 . 2) 0 0 1 2
            ,
        \\some
    );

    try testInterpret(
        \\let foo = \ a b -> a = b in
        \\(foo . 1 . 2) 0 1 1 2
            ,
        \\none
    );
}
