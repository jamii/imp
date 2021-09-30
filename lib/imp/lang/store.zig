const imp = @import("../../imp.zig");
usingnamespace imp.common;
const meta = imp.meta;
const syntax = imp.lang.repr.syntax;
const core = imp.lang.repr.core;
const type_ = imp.lang.repr.type_;

pub const SyntaxMeta = struct {
    start: usize,
    end: usize,
};

pub const CoreMeta = struct {
    from: *const syntax.Expr,
    // this is just useful as a stable id for tests
    id: usize,
};

pub const LogicalMeta = struct {
    from: *const core.Expr,
};

pub const Store = struct {
    arena: *ArenaAllocator,
    syntax_exprs: ArrayList(*const syntax.Expr),
    next_expr_id: usize,
    core_exprs: ArrayList(*const core.Expr),

    pub fn init(arena: *ArenaAllocator) Store {
        return Store{
            .arena = arena,
            .syntax_exprs = ArrayList(*const syntax.Expr).init(&arena.allocator),
            .next_expr_id = 0,
            .core_exprs = ArrayList(*const core.Expr).init(&arena.allocator),
        };
    }

    pub fn putSyntax(self: *Store, expr: syntax.Expr, start: usize, end: usize) !*const syntax.Expr {
        var expr_and_meta = try self.arena.allocator.create(ExprAndMeta(syntax.Expr, SyntaxMeta));
        expr_and_meta.* = .{
            .expr = expr,
            .meta = SyntaxMeta{
                .start = start,
                .end = end,
            },
        };
        try self.syntax_exprs.append(&expr_and_meta.expr);
        return &expr_and_meta.expr;
    }

    pub fn putCore(self: *Store, expr: core.Expr, from: *const syntax.Expr) !*const core.Expr {
        var expr_and_meta = try self.arena.allocator.create(ExprAndMeta(core.Expr, CoreMeta));
        const id = self.next_expr_id;
        self.next_expr_id += 1;
        expr_and_meta.* = .{
            .expr = expr,
            .meta = CoreMeta{
                .from = from,
                .id = id,
            },
        };
        try self.core_exprs.append(&expr_and_meta.expr);
        return &expr_and_meta.expr;
    }

    pub fn putLogical(self: *Store, expr: anytype, from: *const core.Expr) !*const @TypeOf(expr) {
        switch (@TypeOf(expr)) {
            logical.Expr, logical.SetExpr, logical.BoolExpr => {},
            else => @compileError("Not a logical expr: " ++ @typeName(@TypeOf(expr))),
        }
        var expr_and_meta = try self.arena.allocator.create(ExprAndMeta(@TypeOf(expr), CoreMeta));
        expr_and_meta.* = .{
            .expr = expr,
            .meta = LogicalMeta{
                .from = from,
            },
        };
        return &expr_and_meta.expr;
    }

    // TODO using @fieldParentPtr is unnecessarily dangerous

    pub fn getSyntaxMeta(expr: *const syntax.Expr) *const SyntaxMeta {
        return &@fieldParentPtr(ExprAndMeta(syntax.Expr, SyntaxMeta), "expr", expr).meta;
    }

    pub fn getCoreMeta(expr: *const core.Expr) *const CoreMeta {
        return &@fieldParentPtr(ExprAndMeta(core.Expr, CoreMeta), "expr", expr).meta;
    }

    pub fn getLogical(expr: anytype) *const LogicalMeta {
        switch (@TypeOf(expr)) {
            *const logical.Expr, *const logical.SetExpr, *const logical.BoolExpr => {},
            else => @compileError("Not a logical expr: " ++ @typeName(@TypeOf(expr))),
        }
        return &@fieldParentPtr(ExprAndMeta(@TypeOf(expr.*), CoreMeta), "expr", expr).meta;
    }

    pub const SourceSelection = union(enum) {
        Point: usize,
        Range: [2]usize,
    };
    pub fn findSyntaxExprAt(self: Store, selection: SourceSelection) ?*const syntax.Expr {
        if (std.sort.min(*const syntax.Expr, self.syntax_exprs.items, selection, betterMatchForPosition)) |best_match| {
            const match_meta = Store.getSyntaxMeta(best_match);
            switch (selection) {
                .Point => |point| if (match_meta.end <= point) return best_match,
                .Range => |range| if (match_meta.start >= range[0] and match_meta.end <= range[1]) return best_match,
            }
        }
        return null;
    }
    fn betterMatchForPosition(selection: SourceSelection, a: *const syntax.Expr, b: *const syntax.Expr) bool {
        const a_meta = Store.getSyntaxMeta(a);
        const b_meta = Store.getSyntaxMeta(b);
        switch (selection) {
            .Point => |point| {
                // anything past point is not a match
                if (a_meta.end > point) return false;
                if (b_meta.end > point) return true;
                // the expr that ends closest to point is the best match
                if (a_meta.end > b_meta.end) return true;
                if (a_meta.end < b_meta.end) return false;
                // if both end at the same point, the expr that is longest is the best match
                if (a_meta.start < b_meta.start) return true;
                if (a_meta.start > b_meta.start) return false;
            },
            .Range => |range| {
                // anything outside range is not a match
                if (a_meta.start < range[0] or a_meta.end > range[1]) return false;
                if (b_meta.start < range[0] or b_meta.end > range[1]) return true;
                // the expr that is longest is the best match
                if (a_meta.end - a_meta.start > b_meta.end - b_meta.start) return true;
                if (a_meta.end - a_meta.start < b_meta.end - b_meta.start) return false;
            },
        }
        // otherwise, pick arbitrarily
        // TODO pick the expr that is higher in the tree - easy once we switch to ids instead of pointers
        return @ptrToInt(a) > @ptrToInt(b);
    }
};

// --------------------------------------------------------------------------------

fn ExprAndMeta(comptime Expr: type, comptime Meta: type) type {
    return struct {
        expr: Expr,
        meta: Meta,
    };
}
