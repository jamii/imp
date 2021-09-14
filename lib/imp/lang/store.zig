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
    scope: []const ?syntax.Arg,
    // this is just useful as a stable id for tests
    id: usize,
};

pub const LogicalMeta = struct {
    from: *const core.Expr,
};

pub const Store = struct {
    arena: *ArenaAllocator,
    next_expr_id: usize,
    core_exprs: ArrayList(*const core.Expr),
    specializations: DeepHashMap(type_.LazySetType, ArrayList(Specialization)),

    pub fn init(arena: *ArenaAllocator) Store {
        return Store{
            .arena = arena,
            .next_expr_id = 0,
            .core_exprs = ArrayList(*const core.Expr).init(&arena.allocator),
            .specializations = DeepHashMap(type_.LazySetType, ArrayList(Specialization)).init(&arena.allocator),
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

    pub fn putCore(self: *Store, expr: core.Expr, from: *const syntax.Expr, scope: []const ?syntax.Arg) !*const core.Expr {
        var expr_and_meta = try self.arena.allocator.create(ExprAndMeta(core.Expr, CoreMeta));
        const id = self.next_expr_id;
        self.next_expr_id += 1;
        expr_and_meta.* = .{
            .expr = expr,
            .meta = CoreMeta{
                .from = from,
                .scope = scope,
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
    fn betterMatchForPosition(selection: SourceSelection, a: *const core.Expr, b: *const core.Expr) bool {
        const a_core_meta = Store.getCoreMeta(a);
        const a_syntax_meta = Store.getSyntaxMeta(a_core_meta.from);
        const b_core_meta = Store.getCoreMeta(b);
        const b_syntax_meta = Store.getSyntaxMeta(b_core_meta.from);
        switch (selection) {
            .Point => |point| {
                // anything past point is not a match
                if (a_syntax_meta.end > point) return false;
                if (b_syntax_meta.end > point) return true;
                // the expr that ends closest to point is the best match
                if (a_syntax_meta.end > b_syntax_meta.end) return true;
                if (a_syntax_meta.end < b_syntax_meta.end) return false;
                // if both end at the same point, the expr that is longest is the best match
                if (a_syntax_meta.start < b_syntax_meta.start) return true;
                if (a_syntax_meta.start > b_syntax_meta.start) return false;
                // if both have same length, return the outermost expr in the tree
                return a_core_meta.id > b_core_meta.id;
            },
            .Range => |range| {
                // anything outside range is not a match
                if (a_syntax_meta.start < range[0] or a_syntax_meta.end > range[1]) return false;
                if (b_syntax_meta.start < range[0] or b_syntax_meta.end > range[1]) return true;
                // the expr that is longest is the best match
                if (a_syntax_meta.end - a_syntax_meta.start > b_syntax_meta.end - b_syntax_meta.start) return true;
                if (a_syntax_meta.end - a_syntax_meta.start < b_syntax_meta.end - b_syntax_meta.start) return false;
                // if both have the same length, return the outermost expr in the tree
                return a_core_meta.id > b_core_meta.id;
            },
        }
    }
    pub fn findCoreExprAt(self: Store, selection: SourceSelection) ?*const core.Expr {
        if (std.sort.min(*const core.Expr, self.core_exprs.items, selection, betterMatchForPosition)) |best_match| {
            const match_meta = Store.getSyntaxMeta(Store.getCoreMeta(best_match).from);
            switch (selection) {
                .Point => |point| if (match_meta.end <= point) return best_match,
                .Range => |range| if (match_meta.start >= range[0] and match_meta.end <= range[1]) return best_match,
            }
        }
        return null;
    }

    pub fn putSpecialization(self: *Store, lazy: type_.LazySetType, hint: []const type_.ScalarType, set_type: type_.SetType) !void {
        const used_hint = switch (set_type) {
            // always empty so can't say how much of the hint was used
            .None => hint,
            // didn't specialize so all we know is this hint isn't enough
            .Lazy => hint,
            // specialized, so cannot have used >concrete.columns.len of the hint
            .Concrete => |concrete| hint[0..min(hint.len, concrete.columns.len)],
        };
        // analyze shouldn't redo previous work
        assert(self.getSpecialization(lazy, used_hint) == null);
        var slot = try self.specializations.getOrPut(lazy);
        if (!slot.found_existing) slot.value_ptr.* = ArrayList(Specialization).init(&self.arena.allocator);
        try slot.value_ptr.append(.{
            .hint = used_hint,
            .set_type = set_type,
        });
    }

    pub fn getSpecialization(self: *Store, lazy: type_.LazySetType, hint: []const type_.ScalarType) ?type_.SetType {
        const entry = self.specializations.getEntry(lazy) orelse return null;
        for (entry.value_ptr.items) |specialization| {
            const is_match = switch (specialization.set_type) {
                // if couldn't specialize with a longer hint, can't specialize with this one
                .Lazy => specialization.hint.len >= hint.len and meta.deepEqual(specialization.hint[0..hint.len], hint),
                // if could specialize with a shorter hint, can specialize with this one
                .None, .Concrete => specialization.hint.len <= hint.len and meta.deepEqual(specialization.hint, hint[0..specialization.hint.len]),
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
