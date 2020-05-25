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

    pub fn init(arena: *ArenaAllocator) Store {
        return Store{
            .arena = arena,
            .types = DeepHashMap(type_.TypeOf, type_.SetType).init(&arena.allocator),
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

    pub fn putType(self: *Store, expr: *const core.Expr, scope: []const type_.ScalarType, set_type: type_.SetType) ! void {
        _ = try self.types.put(.{.expr = expr, .scope = scope}, set_type);
    }

    pub fn getType(self: *Store, expr: *const core.Expr, scope: []const type_.ScalarType) ?type_.SetType {
        return self.types.getValue(.{.expr = expr, .scope = scope});
    }
};
