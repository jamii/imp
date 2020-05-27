usingnamespace @import("./common.zig");

pub const syntax = @import("./syntax.zig");
pub const core = @import("./core.zig");
pub const type_ = @import("./type.zig");

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
    types: DeepHashMap(type_.TypeOf, type_.SetType),
    next_expr_id: usize,

    pub fn init(arena: *ArenaAllocator) Store {
        return Store{
            .arena = arena,
            .types = DeepHashMap(type_.TypeOf, type_.SetType).init(&arena.allocator),
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

    pub fn putType(self: *Store, expr: *const core.Expr, scope: []const type_.ScalarType, set_type: type_.SetType) ! void {
        _ = try self.types.put(.{.expr = expr, .scope = scope}, set_type);
    }

    pub fn getType(self: *const Store, expr: *const core.Expr, scope: []const type_.ScalarType) ?type_.SetType {
        return self.types.getValue(.{.expr = expr, .scope = scope});
    }
};
