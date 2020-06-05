const imp = @import("../../../imp.zig");
usingnamespace imp.common;
const meta = imp.meta;
const Store = imp.lang.Store;
const core = imp.lang.repr.core;
const type_ = imp.lang.repr.type_;

pub fn analyze(store: *Store, expr: *const core.Expr, error_info: *?ErrorInfo) Error ! type_.SetType {
    var analyzer = Analyzer{
        .store = store,
        .scope = ArrayList(type_.ScalarType).init(&store.arena.allocator),
        .error_info = error_info,
    };
    return analyzer.analyze(expr, &[0]type_.ScalarType{});
}

pub const Error = error {
    // sets error_info
    AnalyzeError,

    // does not set error_info
    OutOfMemory
};

pub const ErrorInfo = struct {
    // TODO want something like a stack of specializations
    message: []const u8,
};

// --------------------------------------------------------------------------------

pub const Analyzer = struct {
    store: *Store,
    scope: ArrayList(type_.ScalarType),
    error_info: *?ErrorInfo,

    fn setError(self: *Analyzer, comptime fmt: []const u8, args: var) Error {
        const message = try format(&self.store.arena.allocator, fmt, args);
        self.error_info.* = ErrorInfo{
            .message = message,
        };
        return error.AnalyzeError;
    }

    fn analyze(self: *Analyzer, expr: *const core.Expr, hint: []const type_.ScalarType) Error ! type_.SetType {
        // TODO need to think about what the key should be
        // hint might vary in length but if > arity(expr) then should return same type
        // maybe only store concrete types?
        // if (self.store.getType(expr, self.scope.items)) |set_type| {
        //     // already analyzed
        //     return set_type;
        // }
        const set_type: type_.SetType = set_type: { switch(expr.*) {
            .None, .Some => {
                break :set_type .{.Concrete = .{
                    .abstract_arity = 0,
                    .columns = &[0]type_.ScalarType{},
                }};
            },
            .Scalar => |scalar| {
                const scalar_type: type_.ScalarType = switch (scalar) {
                    .Text => .Text,
                    .Number => .Number,
                    .Box => imp_panic("Shouldn't be any box literals", .{}),
                };
                break :set_type .{.Concrete = .{
                    .abstract_arity = 0,
                    .columns = try self.dupeScalars(&[1]type_.ScalarType{scalar_type}),
                }};
            },
            .Union, .Intersect => |pair| {
                const left = try self.analyze(pair.left, hint);
                const right = try self.analyze(pair.right, hint);
                if (left == .Lazy or right == .Lazy) {
                    break :set_type .{.Lazy = .{
                        .expr = expr,
                        .scope = try self.dupeScalars(self.scope.items),
                    }};
                }
                if (left.Concrete.columns.len != right.Concrete.columns.len) {
                    return self.setError("Mismatched arities: {} vs {}", .{left.Concrete.columns.len, right.Concrete.columns.len});
                }
                const abstract_arity = max(left.Concrete.abstract_arity, right.Concrete.abstract_arity);
                var columns = try self.store.arena.allocator.alloc(type_.ScalarType, left.Concrete.columns.len);
                for (left.Concrete.columns) |left_type, i| {
                    const right_type = right.Concrete.columns[i];
                    columns[i] = switch (expr.*) {
                        .Union => try self.unionScalar(left_type, right_type),
                        .Intersect => try self.intersectScalar(left_type, right_type),
                        else => unreachable,
                    };
                }
                break :set_type .{.Concrete = .{
                    .abstract_arity = abstract_arity,
                    .columns = columns,
                }};
            },
            .Product => |pair| {
                const left = try self.analyze(pair.left, hint);
                if (left == .Lazy) {
                    break :set_type .{.Lazy = .{
                        .expr=expr,
                        .scope= try self.dupeScalars(self.scope.items)
                    }};
                }
                const right = try self.analyze(pair.right, hint[min(hint.len, left.Concrete.columns.len)..]);
                if (right == .Lazy) {
                    break :set_type .{.Lazy = .{
                        .expr=expr,
                        .scope= try self.dupeScalars(self.scope.items)
                    }};
                }
                const abstract_arity = if (right.Concrete.abstract_arity > 0)
                    left.Concrete.columns.len + right.Concrete.abstract_arity
                    else
                    left.Concrete.abstract_arity;
                var columns = try self.store.arena.allocator.alloc(type_.ScalarType, left.Concrete.columns.len + right.Concrete.columns.len);
                var i: usize = 0;
                for (left.Concrete.columns) |left_type| {
                    columns[i] = left_type;
                    i += 1;
                }
                for (right.Concrete.columns) |right_type| {
                    columns[i] = right_type;
                    i += 1;
                }
                break :set_type .{.Concrete = .{
                    .abstract_arity = abstract_arity,
                    .columns = columns,
                }};
            },
            .Equal => |pair| {
                // the hint for expr doesn't tell us anything about left or right
                const left = try self.analyze(pair.left, &[0]type_.ScalarType{});
                const right = try self.analyze(pair.right, &[0]type_.ScalarType{});
                if (!left.isFinite() or !right.isFinite()) {
                    return self.setError("Cannot equal one or more maybe-infinite sets", .{});
                }
                if (left.Concrete.columns.len != right.Concrete.columns.len) {
                    return self.setError("Mismatched arities: {} vs {}", .{left.Concrete.columns.len, right.Concrete.columns.len});
                }
                for (left.Concrete.columns) |scalar_type, i| {
                    _ = try self.intersectScalar(scalar_type, right.Concrete.columns[i]);
                }
                break :set_type .{.Concrete = .{
                    .abstract_arity = 0,
                    .columns = &[0]type_.ScalarType{},
                }};
            },
            .Name => |name_ix| {
                const scalar_type = self.scope.items[self.scope.items.len - 1 - name_ix];
                break :set_type .{.Concrete = .{
                    .abstract_arity = 0,
                    .columns = try self.dupeScalars(&[1]type_.ScalarType{scalar_type}),
                }};
            },
            .UnboxName => |name_ix| {
                const scalar_type = self.scope.items[self.scope.items.len - 1 - name_ix];
                switch (scalar_type) {
                    .Box => |box_type| {
                        switch (box_type) {
                            .Lazy => |lazy| {
                                // try to specialize
                                const old_scope = self.scope;
                                defer self.scope = old_scope;
                                self.scope = try ArrayList(type_.ScalarType).initCapacity(&self.store.arena.allocator, lazy.scope.len);
                                try self.scope.appendSlice(lazy.scope);
                                break :set_type try self.analyze(lazy.expr, hint);
                            },
                            .Concrete => {
                                break :set_type box_type;
                            }
                        }
                    },
                    else => {
                        return self.setError("Don't know what type will result from unboxing type {}", .{scalar_type});
                    }
                }
            },
            .Negate => |body| {
                // the hint for expr doesn't tell us anything about body
                const body_type = try self.analyze(body,  &[0]type_.ScalarType{});
                if (!body_type.isFinite()) {
                    return self.setError("The body of `!` must have finite type, found {}", .{body_type});
                }
                break :set_type .{.Concrete = .{
                    .abstract_arity = 0,
                    .columns = &[0]type_.ScalarType{},
                }};
            },
            .When => |when| {
                // the hint for expr doesn't tell us anything about condition
                const condition_type = try self.analyze(when.condition, &[0]type_.ScalarType{});
                if (!(condition_type == .Concrete and condition_type.Concrete.columns.len == 0)) {
                    return self.setError("The condition of `when` must have type `maybe`, found {}", .{condition_type});
                }
                break :set_type try self.analyze(when.true_branch, hint);
            },
            .Abstract => |body| {
                if (hint.len == 0) {
                    break :set_type .{.Lazy = .{
                        .expr = expr,
                        .scope = try self.dupeScalars(self.scope.items),
                    }};
                } else {
                    // if we have a hint we can use it to specialize the body
                    try self.scope.append(hint[0]);
                    const body_type = try self.analyze(body, hint[1..]);
                    _ = self.scope.pop();
                    switch (body_type) {
                        .Concrete => |concrete| {
                            const abstract_arity = concrete.abstract_arity + 1;
                            var columns = try ArrayList(type_.ScalarType).initCapacity(&self.store.arena.allocator, 1 + concrete.columns.len);
                            try columns.append(hint[0]);
                            try columns.appendSlice(concrete.columns);
                            break :set_type .{.Concrete = .{
                                .abstract_arity = abstract_arity,
                                .columns = columns.items,
                            }};
                        },
                        .Lazy => {
                            // couldn't fully specialize, give up
                            break :set_type .{.Lazy = .{
                                .expr = expr,
                                .scope = try self.dupeScalars(self.scope.items),
                            }};
                        },
                    }
                }
            },
            .Apply => |pair| {
                // analyze without hints first because we don't know how to split the hint between left and right
                var pair_left = pair.left;
                var pair_right = pair.right;
                var left = try self.analyze(pair.left, &[0]type_.ScalarType{});
                var right = try self.analyze(pair.right, &[0]type_.ScalarType{});
                if (!left.isFinite() and !right.isFinite()) {
                    return self.setError("Cannot apply two maybe-infinite sets: {} vs {}", .{left, right});
                }
                if (!right.isFinite()) {
                    std.mem.swap(*const core.Expr, &pair_left, &pair_right);
                    std.mem.swap(type_.SetType, &left, &right);
                }
                if (left == .Lazy) {
                    // try again but with hints this time
                    var left_hint = try ArrayList(type_.ScalarType).initCapacity(&self.store.arena.allocator, right.Concrete.columns.len + hint.len);
                    try left_hint.appendSlice(right.Concrete.columns);
                    try left_hint.appendSlice(hint);
                    left = try self.analyze(pair_left, left_hint.items);
                }
                if (left == .Lazy) {
                    // couldn't fully specialize, give up
                    break :set_type .{.Lazy = .{
                        .expr = expr,
                        .scope = try self.dupeScalars(self.scope.items),
                    }};
                }
                const joined_arity = min(left.Concrete.columns.len, right.Concrete.columns.len);
                for (left.Concrete.columns[0..joined_arity]) |column, i| {
                    _ = try self.intersectScalar(column, right.Concrete.columns[i]);
                }
                const abstract_arity = if (left.Concrete.abstract_arity > joined_arity)
                    left.Concrete.abstract_arity - joined_arity
                    else
                    0;
                const columns = if (left.Concrete.columns.len > right.Concrete.columns.len)
                    left.Concrete.columns[joined_arity..]
                    else
                    right.Concrete.columns[joined_arity..];
                break :set_type .{.Concrete = .{
                    .abstract_arity = abstract_arity,
                    .columns = columns,
                }};
            },
            .Box => |box| {
                // hint for expr tells us nothing about box body
                const body_type = try self.analyze(box.body, &[0]type_.ScalarType{});
                const box_type = type_.ScalarType{.Box = body_type};
                break :set_type .{.Concrete = .{
                    .abstract_arity = 0,
                    .columns = try self.dupeScalars(&[1]type_.ScalarType{box_type}),
                }};
            },
            .Annotate => |annotate| {
                // TODO some annotations affect types eg solve
                break :set_type try self.analyze(annotate.body, hint);
            },
            .Native => |native| {
                break :set_type .{.Concrete = switch (native) {
                    .Add => .{
                        .abstract_arity = 2,
                        .columns = try self.dupeScalars(&[3]type_.ScalarType{.Number, .Number, .Number}),
                    },
                }};
            },
        }};
        // try self.store.putType(expr, try self.dupeScalars(self.scope.items), set_type);
        return set_type;
    }

    fn dupeScalars(self: *Analyzer, scope: []const type_.ScalarType) Error ! []type_.ScalarType {
        return std.mem.dupe(&self.store.arena.allocator, type_.ScalarType, scope);
    }

    fn unionScalar(self: *Analyzer, a: type_.ScalarType, b: type_.ScalarType) Error ! type_.ScalarType {
        if (meta.deepEqual(a,b)) {
            return a;
        } else {
            return self.setError("TODO type unions are not implemented yet (in unionScalar {} {})", .{a,b});
        }
    }

    fn intersectScalar(self: *Analyzer, a: type_.ScalarType, b: type_.ScalarType) Error ! type_.ScalarType {
        if (meta.deepEqual(a,b)) {
            return a;
        } else {
            return self.setError("Intersection of {} and {} is empty", .{a,b});
        }
    }
};
