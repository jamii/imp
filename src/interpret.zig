usingnamespace @import("./common.zig");

const core = @import("./core.zig");
const value = @import("./value.zig");
const Store = @import("./store.zig").Store;
const type_ = @import("./type.zig");

pub fn interpret(store: *const Store, arena: *ArenaAllocator, expr: *const core.Expr, error_info: *?ErrorInfo) Error ! value.Set {
    var interpreter = Interpreter{
        .store = store,
        .arena = arena,
        .scope = ArrayList(value.Scalar).init(&store.arena.allocator),
        .scope_types = ArrayList(type_.ScalarType).init(&store.arena.allocator),
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

pub const Interpreter = struct {
    store: *const Store,
    arena: *ArenaAllocator,
    scope: ArrayList(value.Scalar),
    scope_types: ArrayList(type_.ScalarType),
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
                    return self.setError(expr, "union of maybe-infinite sets is unimplemented", .{});
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
                    return self.setError(expr, "intersect of maybe-infinite sets is unimplemented", .{});
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
                    return self.setError(expr, "product of maybe-infinite sets is unimplemented", .{});
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
                    return self.setError(expr, "equality of maybe-infinite sets is unimplemented", .{});
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
                        return self.setError(expr, "tried to unbox {}", .{scalar});
                    }
                }
            },
            .When => |when| {
                const condition = try self.interpret(when.condition);
                if (condition == .Abstract) {
                    return self.setError(expr, "when of maybe-infinite sets is unimplemented", .{});
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
                const scope_types = try std.mem.dupe(&self.arena.allocator, type_.ScalarType, self.scope_types.items);
                return value.Set{.Abstract = .{
                    .scope = scope,
                    .scope_types = scope_types,
                    .body = body,
                }};
            },
            .Apply => |pair| {
                const return_type = self.store.getType(expr, self.scope_types.items)
                    orelse panic("Missing type for {} {}", .{expr, self.scope_types.items});
                if (return_type == .Abstract) {
                    const scope = try std.mem.dupe(&self.arena.allocator, value.Scalar, self.scope.items);
                    const scope_types = try std.mem.dupe(&self.arena.allocator, type_.ScalarType, self.scope_types.items);
                    return value.Set{.Abstract = .{
                        .scope = scope,
                        .scope_types = scope_types,
                        .body = expr,
                    }};
                }
                var left = try self.interpret(pair.left);
                var right = try self.interpret(pair.right);
                var left_type = self.store.getType(pair.left, self.scope_types.items)
                    orelse panic("Missing type for {} {}", .{left, self.scope_types.items});
                var right_type = self.store.getType(pair.right, self.scope_types.items)
                    orelse panic("Missing type for {} {}", .{right, self.scope_types.items});
                if (left == .Finite and right == .Finite) {
                    var set = value.FiniteSet.init(&self.arena.allocator);
                    var left_iter = left.Finite.iterator();
                    while (left_iter.next()) |lkv| {
                        var right_iter = right.Finite.iterator();
                        while (right_iter.next()) |rkv| {
                            const min_len = min(lkv.key.len, rkv.key.len);
                            if (meta.deepEqual(lkv.key[0..min_len], rkv.key[0..min_len])) {
                                const tuple = if (lkv.key.len > min_len) lkv.key[min_len..] else rkv.key[min_len..];
                                _ = try set.put(tuple, {});
                            }
                        }
                    }
                    return value.Set{.Finite = set};
                }
                if (right == .Abstract) {
                    std.mem.swap(value.Set, &left, &right);
                    std.mem.swap(type_.SetType, &left_type, &right_type);
                }
                if (right == .Finite and right_type == .Finite) {
                    var set = value.FiniteSet.init(&self.arena.allocator);
                    var right_iter = right.Finite.iterator();
                    while (right_iter.next()) |rkv| {
                        var result = left;
                        for (rkv.key) |arg, i| {
                            const arg_type = right_type.Finite[i];
                            result = try self.interpretApply(result, arg, arg_type);
                        }
                        if (result == .Abstract) {
                            panic("How could this happen?", .{});
                        }
                        var result_iter = result.Finite.iterator();
                        while (result_iter.next()) |kv| {
                            _ = try set.put(kv.key, {});
                        }
                    }
                    return value.Set{.Finite = set};
                } else {
                    return self.setError(expr, "apply of two maybe-infinite sets is unimplemented", .{});
                }
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

    fn interpretApply(self: *Interpreter, fun: value.Set, arg: value.Scalar, arg_type: type_.ScalarType) Error ! value.Set {
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
            .Abstract => |abstract| {
                const old_scope = self.scope;
                defer self.scope = old_scope;
                self.scope = try ArrayList(value.Scalar).initCapacity(&self.arena.allocator, abstract.scope.len);
                try self.scope.appendSlice(abstract.scope);
                try self.scope.append(arg);

                const old_scope_types = self.scope_types;
                defer self.scope_types = old_scope_types;
                self.scope_types = try ArrayList(type_.ScalarType).initCapacity(&self.arena.allocator, abstract.scope_types.len);
                try self.scope_types.appendSlice(abstract.scope_types);
                try self.scope_types.append(arg_type);

                return self.interpret(abstract.body);
            },
        }
    }
};

const parse = if (builtin.is_test) @import("./parse.zig");
const desugar = if (builtin.is_test) @import("./desugar.zig");
const analyze = if (builtin.is_test) @import("./analyze.zig");

fn testInterpret(source: []const u8, expected: []const u8) !void {
    warn("testing {}\n", .{source});
    var arena = ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var store = Store.init(&arena);
    var parse_error_info: ?parse.ErrorInfo = null;
    const syntax_expr = try parse.parse(&store, source, &parse_error_info);
    var desugar_error_info: ?desugar.ErrorInfo = null;
    const core_expr = try desugar.desugar(&store, syntax_expr, &desugar_error_info);
    var analyze_error_info: ?analyze.ErrorInfo = null;
    _ = try analyze.analyze(&store, core_expr, &analyze_error_info);
    var error_info: ?ErrorInfo = null;
    if (interpret(&store, &arena, core_expr, &error_info)) |found| {
        var bytes = ArrayList(u8).init(std.testing.allocator);
        defer bytes.deinit();
        try found.dumpInto(std.testing.allocator, bytes.outStream());
        if (!meta.deepEqual(expected, bytes.items)) {
            panic("\nSource:\n{}\nExpected interpret:\n{}\n\nFound interpret:\n{}", .{source, expected, bytes.items});
        }
    } else |err| {
        warn("\nSource:\n{}\nExpected interpret:\n{}\n\nFound error:\n{}\n", .{source, expected, error_info.?.message});
        return err;
    }
}

fn testInterpretError(source: []const u8, expected: []const u8) !void {
    warn("testing {}\n", .{source});
    var arena = ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var store = Store.init(&arena);
    var parse_error_info: ?parse.ErrorInfo = null;
    const syntax_expr = try parse.parse(&store, source, &parse_error_info);
    var desugar_error_info: ?desugar.ErrorInfo = null;
    const core_expr = try desugar.desugar(&store, syntax_expr, &desugar_error_info);
    var analyze_error_info: ?analyze.ErrorInfo = null;
    _ = try analyze.analyze(&store, core_expr, &analyze_error_info);
    var error_info: ?ErrorInfo = null;
    if (interpret(&store, &arena, core_expr, &error_info)) |found| {
        var bytes = ArrayList(u8).init(std.testing.allocator);
        try found.dumpInto(std.testing.allocator, bytes.outStream());
        panic("\nSource:\n{}\nExpected error:\n{}\n\nFound interpret:\n{}", .{source, expected, bytes.items});
    } else |err| {
        if (!meta.deepEqual(expected, error_info.?.message)) {
            warn("\nSource:\n{}\nExpected error:\n{}\n\nFound error:\n{}\n", .{source, expected, error_info.?.message});
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
        \\[5777665765994302375;]
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
        \\[5777665765994302375;]
    );

    // TODO this is a problem
    try testInterpret(
        \\let foo = \ a -> [\ b -> a . b] in
        \\(foo 1) | (foo 2)
            ,
        \\[5777665765994302375;] | [5777665765994302375;]
    );

    try testInterpret(
        \\let foo = \ a -> [\ b -> a . b] in
        \\(1 . (foo 1)) | (2 . (foo 2))
            ,
        \\1 . [12439490446328561653; 1] | 2 . [12439490446328561653; 2]
    );

    try testInterpret(
        \\let foo = \ a -> [\ b -> a . b] in
        \\let bar = (1 . (foo 1)) | (2 . (foo 2)) in
        \\bar \ a [f] -> a . (f 41)
            ,
        \\any . any . any
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
