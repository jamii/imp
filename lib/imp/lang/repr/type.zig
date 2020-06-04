const imp = @import("../../../imp.zig");
usingnamespace imp.common;
const meta = imp.meta;
const Store = imp.lang.Store;
const value = imp.lang.repr.value;
const core = imp.lang.repr.core;

pub const SetType = union(enum) {
    /// A set with these column types
    Concrete: ConcreteSetType,
    /// Something that we can't type until it's specialized
    Lazy: LazySetType,

    pub fn isFinite(self: SetType) bool {
        return self == .Concrete and self.Concrete.abstract_arity = 0;
    }

    pub fn dumpInto(self: SetType, out_stream: var) anyerror!void {
        switch (self) {
            .Concrete => |concrete| {
                if (concrete.columns.len == 0) {
                    try out_stream.writeAll("maybe");
                } else {
                    for (concrete.columns) |scalar_type, i| {
                        try out_stream.writeAll(
                            if (i == 0) ""
                                else if (i <= concrete.abstract_arity) " -> "
                                else " . ");
                        try scalar_type.dumpInto(out_stream);
                    }
                }
            },
            .Lazy => |lazy| {
                try lazy.dumpInto(out_stream);
            },
        }
    }
};

pub const ConcreteSetType = struct {
    abstract_arity: usize,
    columns: []ScalarType,
};

pub const LazySetType = struct {
    expr: *const core.Expr,
    scope: []const ScalarType,

    // Equality on expr id and scope value

    pub fn deepHashInto(hasher: var, self: TypeOf) void {
        hasher.update(std.mem.asBytes(&@ptrToInt(self.expr)));
        meta.deepHashInto(hasher, self.scope);
    }

    pub fn deepCompare(self: TypeOf, other: TypeOf) meta.Ordering {
        const ordering = meta.deepCompare(Store.getCoreMeta(self.expr).id, Store.getCoreMeta(other.expr).id);
        if (ordering != .Equal) return ordering;
        return meta.deepCompare(self.scope, other.scope);
    }

    pub fn dumpInto(self: TypeOf, out_stream: var) anyerror!void {
        try std.fmt.format(out_stream, "type_of(expr {};", .{Store.getCoreMeta(self.abstract).id});
        for (self.scope) |scalar_type, i| {
            try out_stream.writeAll(if (i == 0) " " else " . ");
            try self.item.dumpInto(out_stream);
        }
        try out_stream.writeAll(")");
    }
};

pub const ScalarType = union(enum) {
    Text,
    Number,
    Box: SetType,

    pub fn dumpInto(self: ScalarType, out_stream: var) anyerror!void {
        switch (self) {
            .Any => {
                try out_stream.writeAll("any");
            },
            .Box => |set_type| {
                try out_stream.writeAll("[");
                try set_type.dumpInto(out_stream);
                try out_stream.writeAll("]");
            },
        }
    }
};
