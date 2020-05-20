usingnamespace @import("./common.zig");

pub const syntax_ = @import("./syntax.zig");
pub const core_ = @import("./core.zig");

pub const Store = struct {
    arena: *ArenaAllocator,

    pub fn init(arena: *ArenaAllocator) Store {
        return Store{
            .arena = arena,
        };
    }

    pub fn syntax(self: *Store, expr: syntax_.Expr) ! *const syntax_.Expr {
        var stored_expr = try self.arena.allocator.create(syntax_.Expr);
        stored_expr.* = expr;
        return stored_expr;
    }

    pub fn core(self: *Store, expr: core_.Expr) ! *const core_.Expr {
        var stored_expr = try self.arena.allocator.create(core_.Expr);
        stored_expr.* = expr;
        return stored_expr;
    }
};
