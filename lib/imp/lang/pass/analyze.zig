const imp = @import("../../../imp.zig");
usingnamespace imp.common;
const Store = imp.lang.store.Store;
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
                    return self.setError("TODO cannot product two maybe-infinite sets", .{});
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
                    return self.setError("Cannot = two maybe-infinite sets", .{});
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
                _ = try self.analyze(when.condition, type_.SetType{.Finite = &[0]type_.ScalarType{}});
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
            .Native => |native| {
                break :set_type .{.Abstract = .{
                    .expr = expr,
                    // natives don't close over anything
                    .scope = &[0]type_.ScalarType{},
                }};
            },
        }};
        // TODO is this necessary?
        // if (hint_o) |hint| {
        //     if (!hint.isSuperTypeOrEqual(set_type)) {
        //         return self.setError("Expected {}, found {}", .{hint, set_type});
        //     }
        // }
        dump(.{"type", expr, self.scope.items, set_type});
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
                    .Native => |native| {
                        TODO();
                    },
                    .Union, .Intersect, .Product => |pair| {
                        TODO();
                    },
                    else => panic("What are this {}", .{abstract_type.expr}),
                }
            },
        }
    }
};

fn testAnalyze(source: []const u8, expected: []const u8) !void {
 var arena = ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var store = Store.init(&arena);
    var parse_error_info: ?parse.ErrorInfo = null;
    const syntax_expr = try parse.parse(&store, source, &parse_error_info);
    var desugar_error_info: ?desugar.ErrorInfo = null;
    const core_expr = try desugar.desugar(&store, syntax_expr, &desugar_error_info);
    var error_info: ?ErrorInfo = null;
    if (analyze(&store, core_expr, &error_info)) |found| {
        var bytes = ArrayList(u8).init(std.testing.allocator);
        defer bytes.deinit();
        try found.dumpInto(bytes.outStream());
        if (!meta.deepEqual(expected, bytes.items)) {
            panic("\nSource:\n{}\nExpected analyze:\n{}\n\nFound analyze:\n{}", .{source, expected, bytes.items});
        }
    } else |err| {
        warn("\nSource:\n{}\nExpected analyze:\n{}\n\nFound error:\n{}\n", .{source, expected, error_info.?.message});
        return err;
    }
}

const parse = if (builtin.is_test) @import("./parse.zig");
const desugar = if (builtin.is_test) @import("./desugar.zig");

fn testAnalyzeError(source: []const u8, expected: []const u8) !void {
    var arena = ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var store = Store.init(&arena);
    var parse_error_info: ?parse.ErrorInfo = null;
    const syntax_expr = try parse.parse(&store, source, &parse_error_info);
    var desugar_error_info: ?desugar.ErrorInfo = null;
    const core_expr = try desugar.desugar(&store, syntax_expr, &desugar_error_info);
    var error_info: ?ErrorInfo = null;
    if (analyze(&store, core_expr, &error_info)) |found| {
        var bytes = ArrayList(u8).init(std.testing.allocator);
        try found.dumpInto(bytes.outStream());
        panic("\nSource:\n{}\nExpected error:\n{}\n\nFound analyze:\n{}", .{source, expected, bytes.items});
    } else |err| {
        if (!meta.deepEqual(expected, error_info.?.message)) {
            warn("\nSource:\n{}\nExpected error:\n{}\n\nFound error:\n{}\n", .{source, expected, error_info.?.message});
            return err;
        }
    }
}

test "analyze" {
    try testAnalyze(
        \\1
            ,
        \\any
    );

    try testAnalyze(
        \\1 . "foo"
            ,
        \\any . any
    );

    try testAnalyze(
        \\1 . "foo" | 2 . "bar"
            ,
        \\any . any
    );

    try testAnalyze(
        \\1 . ("foo" | 2) . "bar"
            ,
        \\any . any . any
    );

    try testAnalyzeError(
        \\1 . "foo" | 2
            ,
        \\Mismatched arities: 2 vs 1
    );

    try testAnalyze(
        \\!(1 . "foo")
            ,
        ""
    );

    try testAnalyze(
        \\when some then 1 . 2
            ,
        \\any . any
    );

    try testAnalyze(
        \\when none then 1 . 2
            ,
        \\any . any
    );

    try testAnalyze(
        \\let a = 1 . "foo" in
        \\a . a
            ,
        \\any . any . any . any
    );

    try testAnalyze(
        \\let foo = \ a b -> a = b in
        \\foo 1
            ,
        \\type_of(16845918103739231216; any)
    );

     try testAnalyze(
        \\let foo = \ a b -> a = b in
        \\foo 1 2
            ,
        ""
    );

    try testAnalyze(
        \\let a = [1 . "foo"] in
        \\a \ [a] -> a . a
            ,
        \\any . any . any . any
    );

    try testAnalyze(
        \\let foo = \ a -> [\ b -> a . b] in
        \\foo 1
            ,
        \\[type_of(12439490446328561653; any)]
    );

    try testAnalyze(
        \\let foo = \ a -> [\ b -> a . b] in
        \\(foo 1) | (foo 2)
            ,
        \\[type_of(12439490446328561653; any)]
    );

    try testAnalyze(
        \\let foo = \ a -> [\ b -> a . b] in
        \\(1 . (foo 1)) | (2 . (foo 2))
            ,
        \\any . [type_of(12439490446328561653; any)]
    );

    try testAnalyze(
        \\let foo = \ a -> [\ b -> a . b] in
        \\let bar = (1 . (foo 1)) | (2 . (foo 2)) in
        \\bar \ a [f] -> a . (f 41)
            ,
        \\any . any . any
    );

    try testAnalyzeError(
        \\let foo1 = \ a -> [\ b -> a . b] in
        \\let foo2 = \ a -> [\ b -> a . b] in
        \\let bar = (1 . (foo1 1)) | (2 . (foo2 2)) in
        \\bar \ a [f] -> a . (f 41)
            ,
        \\Don't know what type will result from unboxing type ScalarType{ .Any = void }
    );

    try testAnalyze(
        \\let foo = 1 . 1 | 2 . 2 in
        \\foo foo
            ,
        ""
    );

    try testAnalyzeError(
        \\let foo = \ a b -> a = b in
        \\foo foo
            ,
        \\Cannot apply two maybe-infinite sets
    );

    try testAnalyzeError(
        \\let foo = \ a b -> a = b in
        \\foo = foo
            ,
        \\Cannot = two maybe-infinite sets
    );

    try testAnalyzeError(
        \\let foo = \ a b -> a = b in
        \\foo | 1 . 2
            ,
        \\TODO cannot union two maybe-infinite sets
    );
}
