const imp = @import("../../imp.zig");
usingnamespace imp.common;
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

pub fn ExprAndMeta(comptime Expr: type, comptime Meta: type) type {
    return struct {
        expr: Expr,
        meta: Meta,
    };
}

pub const Store = struct {
    arena: *ArenaAllocator,
    next_expr_id: usize,

    pub fn init(arena: *ArenaAllocator) Store {
        return Store{
            .arena = arena,
            // .types = DeepHashMap(type_.TypeOf, type_.SetType).init(&arena.allocator),
            .next_expr_id = 0,
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
        const id = self.next_expr_id;
        self.next_expr_id += 1;
        expr_and_meta.* = .{
            .expr = expr,
            .meta = CoreMeta {
                .from = from,
                .id = id,
            }
        };
        return &expr_and_meta.expr;
    }

    pub fn getSyntaxMeta(expr: *const syntax.Expr) *const SyntaxMeta {
        return &@fieldParentPtr(ExprAndMeta(syntax.Expr, SyntaxMeta), "expr", expr).meta;
    }

    pub fn getCoreMeta(expr: *const core.Expr) *const CoreMeta {
        return &@fieldParentPtr(ExprAndMeta(core.Expr, CoreMeta), "expr", expr).meta;
    }
};
