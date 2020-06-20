const imp = @import("../../../imp.zig");
usingnamespace imp.common;
const meta = imp.meta;
const value = imp.lang.repr.value;
const core = imp.lang.repr.core;
const logical = imp.lang.repr.logical;

pub fn flatten(store: *Store, core_expr: *const core.Expr, error_info: *?ErrorInfo) Error ! *const logical.Exprs {
    var flattener = Flattener{
        .store = store,
        .current_expr = null,
        .exprs = ArrayList(logical.expr).init(&store.arena.allocator),
        .error_info = error_info,
    };
    const args = TODO();
    const root = try flattener.flatten(core_expr, args);
    try flattener.putExpr(.{.Set = .{args = args, .body = root}});
    return .{.exprs = flattener.exprs.items};
}

pub const Error = error {
    // sets error_info
    FlattenError,

    // does not set error_info
    OutOfMemory,
};

pub const ErrorInfo = struct {
    expr: ?*const syntax.Expr,
    message: []const u8,
};

// --------------------------------------------------------------------------------

const Flattener = struct {
    store: *Store,
    current_expr: *const core.Expr,
    exprs: ArrayList(logical.Expr),
    error_info: *?ErrorInfo,

    fn setError(self: *Flattener, comptime fmt: []const u8, args: var) Error {
        const message = try format(&self.store.arena.allocator, fmt, args);
        self.error_info.* = ErrorInfo{
            .expr = self.current_expr,
            .message = message,
        };
        return error.FlattenError;
    }

    fn flatten(self: *Flattener, core_expr: *const core.Expr, args: []const NameIx) Error ! logical.BoolExpr {
        const prev_expr = self.current_expr;
        self.current_expr = core_expr;
        defer self.current_expr = prev_expr;
        switch (core_expr.*) {
            else => TODO(),
        }
    }
}
