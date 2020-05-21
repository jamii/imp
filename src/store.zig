usingnamespace @import("./common.zig");

pub const syntax = @import("./syntax.zig");
pub const core = @import("./core.zig");

pub const SyntaxMeta = struct {
    start: usize,
    end: usize,
};

pub const CoreMeta = struct {
    from: *const syntax.Expr,
};

pub fn ExprAndMeta(comptime Expr: type, comptime Meta: type) type {
    return struct {
        expr: Expr,
        meta: Meta,
    };
}

pub const Store = struct {
    arena: *ArenaAllocator,
    box_exprs: ArrayList(*const core.Expr),

    pub fn init(arena: *ArenaAllocator) Store {
        return Store{
            .arena = arena,
            .box_exprs = ArrayList(*const core.Expr).init(&arena.allocator),
        };
    }

    pub fn putSyntax(self: *Store, expr: syntax.Expr, start: usize, end: usize) ! *const syntax.Expr {
        var expr_and_meta = try self.arena.allocator.create(ExprAndMeta(syntax.Expr, SyntaxMeta));
        expr_and_meta.* = .{
            .expr = expr,
            .meta = SyntaxMeta {
                .start = start,
                .end = end,
            }
        };
        return &expr_and_meta.expr;
    }

    pub fn putCore(self: *Store, expr: core.Expr, from: *const syntax.Expr) ! *const core.Expr {
        var expr_and_meta = try self.arena.allocator.create(ExprAndMeta(core.Expr, CoreMeta));
        expr_and_meta.* = .{
            .expr = expr,
            .meta = CoreMeta {
                .from = from,
            }
        };
        return &expr_and_meta.expr;
    }

    pub fn getSyntaxMeta(self: *Store, expr: *const Syntax.Expr) *SyntaxMeta {
        return &@fieldParentPtr(ExprAndMeta(syntax.Expr, SyntaxMeta), "expr", expr).meta;
    }

    pub fn getCoreMeta(self: *Store, expr: *const Core.Expr) *CoreMeta {
        return &@fieldParentPtr(ExprAndMeta(core.Expr, CoreMeta), "expr", expr).meta;
    }

    pub fn putBox(self: *Store, expr: *const core.Expr) ! core.BoxId {
        try self.box_exprs.append(expr);
        return self.box_exprs.items.len - 1;
    }
};
