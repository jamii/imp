usingnamespace @import("./common.zig");

pub const syntax = @import("./syntax.zig");
pub const core = @import("./core.zig");

pub const SyntaxMeta = struct {
    expr: syntax.Expr,
    start: usize,
    end: usize,
};

pub const CoreMeta = struct {
    expr: core.Expr,
    from: *const syntax.Expr,
};

pub const Store = struct {
    arena: *ArenaAllocator,
    box_exprs: ArrayList(*const core.Expr),

    pub fn init(arena: *ArenaAllocator) Store {
        return Store{
            .arena = arena,
            .box_exprs = ArrayList(*const core.Expr).init(&arena.allocator),
        };
    }

    pub fn put_syntax(self: *Store, expr: syntax.Expr, start: usize, end: usize) ! *const syntax.Expr {
        var expr_meta = try self.arena.allocator.create(SyntaxMeta);
        expr_meta.* = SyntaxMeta {
            .expr = expr,
            .start = start,
            .end = end,
        };
        return &expr_meta.expr;
    }

    pub fn put_core(self: *Store, expr: core.Expr, from: *const syntax.Expr) ! *const core.Expr {
        var expr_meta = try self.arena.allocator.create(CoreMeta);
        expr_meta.* = SyntaxMeta {
            .expr = expr,
            .from = from,
        };
        return &expr_meta.expr;
    }

    pub fn get_syntax_meta(self: *Store, expr: *const Syntax.Expr) *const SyntaxMeta {
        return @fieldParentPtr(SyntaxMeta, "expr", expr);
    }

    pub fn get_core_meta(self: *Store, expr: *const Core.Expr) *const CoreMeta {
        return @fieldParentPtr(CoreMeta, "expr", expr);
    }

    pub fn put_box(self: *Store, expr: *const core.Expr) ! core.BoxId {
        try self.box_exprs.append(expr);
        return self.box_exprs.items.len - 1;
    }
};
