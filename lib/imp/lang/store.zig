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

pub const Store = struct {
    arena: *ArenaAllocator,
    next_expr_id: usize,
    specializations: DeepHashMap(type_.LazySetType, ArrayList(Specialization)),

    pub fn init(arena: *ArenaAllocator) Store {
        return Store{
            .arena = arena,
            .specializations = DeepHashMap(type_.LazySetType, ArrayList(Specialization)).init(&arena.allocator),
            .next_expr_id = 0,
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
        return &expr_and_meta.expr;
    }

    pub fn getSyntaxMeta(expr: *const syntax.Expr) *const SyntaxMeta {
        return &@fieldParentPtr(ExprAndMeta(syntax.Expr, SyntaxMeta), "expr", expr).meta;
    }

    pub fn getCoreMeta(expr: *const core.Expr) *const CoreMeta {
        return &@fieldParentPtr(ExprAndMeta(core.Expr, CoreMeta), "expr", expr).meta;
    }

    pub fn putSpecialization(self: *Store, lazy: type_.LazySetType, hint: []const type_.ScalarType, set_type: type_.SetType) !void {
        const used_hint = switch (set_type) {
            // didn't specialize so all we know is this hint isn't enough
            .Lazy => hint,
            // specialized, so cannot have used >concrete.columns.len of the hint
            .Concrete => |concrete| hint[0..min(hint.len, concrete.columns.len)],
        };
        // analyze shouldn't redo previous work
        assert(self.getSpecialization(lazy, used_hint) == null);
        var slot = try self.specializations.getOrPut(lazy);
        if (!slot.found_existing) slot.entry.value = ArrayList(Specialization).init(&self.arena.allocator);
        try slot.entry.value.append(.{
            .hint = used_hint,
            .set_type = set_type,
        });
    }

    pub fn getSpecialization(self: *Store, lazy: type_.LazySetType, hint: []const type_.ScalarType) ?type_.SetType {
        const kv = self.specializations.getEntry(lazy) orelse return null;
        for (kv.value.items) |specialization| {
            const is_match = switch (specialization.set_type) {
                // if couldn't specialize with a longer hint, can't specialize with this one
                .Lazy => specialization.hint.len >= hint.len and meta.deepEqual(specialization.hint[0..hint.len], hint),
                // if could specialize with a shorter hint, can specialize with this one
                .Concrete => specialization.hint.len <= hint.len and meta.deepEqual(specialization.hint, hint[0..specialization.hint.len]),
            };
            if (is_match) return specialization.set_type;
        }
        return null;
    }
};

// --------------------------------------------------------------------------------

fn ExprAndMeta(comptime Expr: type, comptime Meta: type) type {
    return struct {
        expr: Expr,
        meta: Meta,
    };
}

const Specialization = struct {
    hint: []const type_.ScalarType,
    set_type: type_.SetType,
};
