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
    return analyzer.analyze(expr, null);
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

    pub fn setError(self: *Analyzer, comptime fmt: []const u8, args: var) Error {
        const message = try format(&self.store.arena.allocator, fmt, args);
        self.error_info.* = ErrorInfo{
            .message = message,
        };
        return error.AnalyzeError;
    }

    pub fn dupeScope(self: *Analyzer, scope: []const type_.ScalarType) Error ! []type_.ScalarType {
        return std.mem.dupe(&self.store.arena.allocator, type_.ScalarType, scope);
    }

    pub fn analyze(self: *Analyzer, expr: *const core.Expr, hint_o: ?type_.SetType) Error ! type_.SetType {
        if (self.store.getType(expr, self.scope.items)) |set_type| {
            // already analyzed
            return set_type;
        }
        const set_type: type_.SetType = set_type: { switch(expr.*) {
            .None => {
                if (hint_o) |hint| {
                    break :set_type hint;
                } else {
                    return self.setError("Cannot infer type for none without hint", .{});
                }
            },
            .Some => {
                break :set_type .{.Finite = &[0]type_.ScalarType{}};
            },
            .Scalar => {
                // there are no box literals so don't have to check type
                break :set_type .{.Finite = try self.dupeScope(&[1]type_.ScalarType{.Any})};
            },
            .Union => |pair| {
                const left = try self.analyze(pair.left, hint_o);
                const right = try self.analyze(pair.right, hint_o orelse left);
                if (left == .Abstract or right == .Abstract) {
                    return self.setError("TODO cannot union two maybe-infinite sets", .{});
                }
                if (left.Finite.len != right.Finite.len) {
                    return self.setError("Mismatched arities: {} vs {}", .{left.Finite.len, right.Finite.len});
                }
                var columns = try self.store.arena.allocator.alloc(type_.ScalarType, left.Finite.len);
                for (left.Finite) |left_type, i| {
                    const right_type = right.Finite[i];
                    columns[i] = type_.ScalarType.union_(left_type, right_type);
                }
                break :set_type .{.Finite = columns};
            },
            .Intersect => |pair| {
                const left = try self.analyze(pair.left, hint_o);
                const right = try self.analyze(pair.right, hint_o orelse left);
                if (left == .Abstract or right == .Abstract) {
                   return self.setError("TODO cannot intersect two maybe-infinite sets", .{});
                }
                if (left.Finite.len != right.Finite.len) {
                    return self.setError("Mismatched arities: {} vs {}", .{left.Finite.len, right.Finite.len});
                }
                var columns = try self.store.arena.allocator.alloc(type_.ScalarType, left.Finite.len);
                for (left.Finite) |left_type, i| {
                    const right_type = right.Finite[i];
                    columns[i] = type_.ScalarType.intersect(left_type, right_type);
                }
                break :set_type .{.Finite = columns};
            },
            .Product => |pair| {
                // no way to figure out how to split hint
                const left = try self.analyze(pair.left, null);
                const right = try self.analyze(pair.right, null);
                if (left == .Abstract or right == .Abstract) {
                    return self.setError("TODO cannot product one or more maybe-infinite sets", .{});
                }
                var columns = try self.store.arena.allocator.alloc(type_.ScalarType, left.Finite.len + right.Finite.len);
                var i: usize = 0;
                for (left.Finite) |left_type| {
                    columns[i] = left_type;
                    i += 1;
                }
                for (right.Finite) |right_type| {
                    columns[i] = right_type;
                    i += 1;
                }
                break :set_type .{.Finite = columns};
            },
            .Equal => |pair| {
                // hint should be truthy, doesn't give info on left/right types
                const left = try self.analyze(pair.left, null);
                const right = try self.analyze(pair.right, left);
                if (left == .Abstract or right == .Abstract) {
                    return self.setError("Cannot equal two maybe-infinite sets", .{});
                }
                break :set_type .{.Finite = &[0]type_.ScalarType{}};
            },
            .Name => |name_ix| {
                const scalar_type = self.scope.items[self.scope.items.len - 1 - name_ix];
                break :set_type .{.Finite = try self.dupeScope(&[1]type_.ScalarType{scalar_type})};
            },
            .UnboxName => |name_ix| {
                const scalar_type = self.scope.items[self.scope.items.len - 1 - name_ix];
                switch (scalar_type) {
                    .Box => |box_type| {
                        break :set_type box_type;
                    },
                    else => {
                        return self.setError("Don't know what type will result from unboxing type {}", .{scalar_type});
                    }
                }
            },
            .When => |when| {
                const condition_type = try self.analyze(when.condition, type_.SetType{.Finite = &[0]type_.ScalarType{}});
                if (!(condition_type == .Finite and condition_type.Finite.len == 0)) {
                    return self.setError("The condition of `when` must have type `some`, found {}", .{condition_type});
                }
                break :set_type try self.analyze(when.true_branch, hint_o);
            },
            .Abstract => |body| {
                // TODO should we restrict scope to closed over?
                break :set_type .{.Abstract = .{
                    .expr = expr,
                    .scope = try self.dupeScope(self.scope.items),
                }};
            },
            .Apply => |pair| {
                // no way to figure out how to split hint
                var left = try self.analyze(pair.left, null);
                var right = try self.analyze(pair.right, null);
                if (left == .Abstract and right == .Abstract) {
                    return self.setError("Cannot apply two maybe-infinite sets", .{});
                }
                if (left == .Abstract) {
                    std.mem.swap(type_.SetType, &left, &right);
                }
                var result = right;
                for (left.Finite) |scalar_type| {
                    result = try self.analyzeApply(result, scalar_type);
                }
                break :set_type result;                                             },
            .Box => |box| {
                // TODO should we restrict scope to closed over?
                const body_type = try self.analyze(box.body, null);
                const box_type = type_.ScalarType{.Box = body_type};
                break :set_type .{.Finite = try self.dupeScope(&[1]type_.ScalarType{box_type})};
            },
            .Annotate => |annotate| {
                // TODO some annotations affect types eg solve
                break :set_type try self.analyze(annotate.body, hint_o);
            },
            // .Native => |native| {
            //     break :set_type .{.Abstract = .{
            //         .expr = expr,
            //         // natives don't close over anything
            //         .scope = &[0]type_.ScalarType{},
            //     }};
            // },
        }};
        // TODO in combination with union hints this breaks type unification
        // if (hint_o) |hint| {
        //     if (!set_type.isSubTypeOrEqual(hint)) {
        //         return self.setError("Expected {}, found {}", .{hint, set_type});
        //     }
        // }
        try self.store.putType(expr, try self.dupeScope(self.scope.items), set_type);
        return set_type;
    }

    fn analyzeApply(self: *Analyzer, set_type: type_.SetType, arg_type: type_.ScalarType) Error ! type_.SetType {
        switch (set_type) {
            .Finite => |finite_type| {
                if (finite_type.len >= 1) {
                    return type_.SetType{.Finite = finite_type[1..]};
                } else {
                    return type_.SetType{.Finite = try self.dupeScope(&[1]type_.ScalarType{arg_type})};
                }
            },
            .Abstract => |abstract_type| {
                const old_scope = self.scope;
                defer self.scope = old_scope;
                self.scope = try ArrayList(type_.ScalarType).initCapacity(&self.store.arena.allocator, abstract_type.scope.len + 1);
                try self.scope.appendSlice(abstract_type.scope);
                try self.scope.append(arg_type);
                switch (abstract_type.expr.*) {
                    .Abstract => |body| {
                        return self.analyze(body, null);
                    },
                    // .Native => |native| {
                    //     TODO();
                    // },
                    .Union, .Intersect, .Product => |pair| {
                        TODO();
                    },
                    else => imp_panic("What are this {}", .{abstract_type.expr}),
                }
            },
        }
    }
};
