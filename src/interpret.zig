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

pub const Interpreter = struct {
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
                _ = try set.put(&[1]value.Scalar{scalar}, {});
                return value.Set{.Finite = set};
            },
            .Union => |pair| {
                const left = try self.interpret(pair.left);
                const right = try self.interpret(pair.right);
                if (left.isFinite() and right.isFinite()) {
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
                if (left.isFinite() and right.isFinite()) {
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
                if (left.isFinite() and right.isFinite()) {
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
                if (left.isFinite() and right.isFinite()) {
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
                _ = try set.put(&[1]value.Scalar{scalar}, {});
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
                if (!condition.isFinite()) {
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
                return value.Set{.Abstract = .{
                    .scope = scope,
                    .body = body,
                }};
            },
            .Apply => |pair| {
                var left = try self.interpret(pair.left);
                var right = try self.interpret(pair.right);
                if (left.isFinite() and right.isFinite()) {
                    var set = value.FiniteSet.init(&self.arena.allocator);
                    var leftIter = left.Finite.iterator();
                    while (leftIter.next()) |lkv| {
                        var rightIter = right.Finite.iterator();
                        while (rightIter.next()) |rkv| {
                            const min_len = min(lkv.key.len, rkv.key.len);
                            if (meta.deepEqual(lkv.key[0..min_len], rkv.key[0..min_len])) {
                                const tuple = if (lkv.key.len > min_len) lkv.key[min_len..] else rkv.key[min_len..];
                                _ = try set.put(tuple, {});
                            }
                        }
                    }
                    return value.Set{.Finite = set};
                }
                if (!right.isFinite()) {
                    std.mem.swap(value.Set, &left, &right);
                }
                if (right.isFinite()) {
                    const old_scope = self.scope;
                    self.scope = try ArrayList(value.Scalar).initCapacity(&self.arena.allocator, right.Abstract.scope.len);
                    try self.scope.appendSlice(right.Abstract.scope);
                    var iter = right.Finite.iterator();
                    while (iter.next()) |kv| {
                    }
                    self.scope = old_scope;
                    TODO();
                } else {
                    return self.setError(expr, "apply of two maybe-infinite sets is unimplemented", .{});
                }
            },
            else => TODO(),
        }
    }
};

test "interpret" {
    std.meta.refAllDecls(@This());
}
