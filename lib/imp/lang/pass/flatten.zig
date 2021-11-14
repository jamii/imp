const imp = @import("../../../imp.zig");
const u = imp.util;
const value = imp.lang.repr.value;
const core = imp.lang.repr.core;
const logical = imp.lang.repr.logical;

pub fn flatten(store: *Store, expr: *const core.Expr, error_info: *?ErrorInfo) Error!*const logical.Exprs {
    var flattener = Flattener{
        .store = store,
        .exprs = ArrayList(logical.Expr).init(&store.arena.allocator),
        .error_info = error_info,
    };
    const args = TODO();
    const root = try flattener.flatten(expr, args);
    try flattener.putExpr(.{ .Collect = .{ .args = args, .body = root } });
    return .{ .exprs = flattener.exprs.items };
}

pub const Error = error{
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
    exprs: ArrayList(logical.Expr),
    error_info: *?ErrorInfo,

    fn setError(self: *Flattener, expr: *const core.Expr, comptime fmt: []const u8, args: anytype) Error {
        const message = try formatToString(&self.store.arena.allocator, fmt, args);
        self.error_info.* = ErrorInfo{
            .expr = self.current_expr,
            .message = message,
        };
        return error.FlattenError;
    }

    /// Store the logical version of expr and return a reference to it
    fn flatten(self: *Flattener, expr: *const core.Expr) Error!logical.ScalarRef {
        switch (expr.*) {
            else => TODO(),
        }
    }

    /// Return the result of applying expr to args
    /// (May have to flatten some children of expr)
    fn applyTo(self: *Flattener, expr: *const core.Expr, args: []const NameIx) Error!logical.BoolExpr {
        switch (expr.*) {
            else => TODO(),
        }
    }
};
