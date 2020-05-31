const imp = @import("../../../imp.zig");
usingnamespace imp.common;
const meta = imp.meta;
const Store = imp.lang.Store;
const value = imp.lang.repr.value;
const core = imp.lang.repr.core;

pub const ScalarType = union(enum) {
    Any,
    Box: SetType,

    pub fn union_(a: ScalarType, b: ScalarType) ScalarType {
        if (a == .Box and b == .Box and meta.deepEqual(a.Box, b.Box)) {
            return a;
        } else {
            return .Any;
        }
    }

    pub fn intersect(a: ScalarType, b: ScalarType) ScalarType {
        // TODO should unequal boxes really return Any?
        if (a == .Box and b == .Box and meta.deepEqual(a.Box, b.Box)) {
            return a;
        } else {
            return .Any;
        }
    }

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

/// Represents whatever the type of this expr with this scope is
pub const TypeOf = struct {
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
        // TODO figure out a better way to name these
        try std.fmt.format(out_stream, "type_of({};", .{Store.getCoreMeta(self.expr).id});
        if (self.scope.len > 0) {
            try out_stream.writeAll(" ");
            try self.scope[0].dumpInto(out_stream);
            for (self.scope[1..]) |scalar_type| {
                try out_stream.writeAll(" . ");
                try scalar_type.dumpInto(out_stream);
            }
        }
        try out_stream.writeAll(")");
    }
};

pub const SetType = union(enum) {
    /// A finite set with these column types
    Finite: []const ScalarType,
    /// Something that we can't type until it's specialized
    Abstract: TypeOf,

    pub fn dumpInto(self: SetType, out_stream: var) anyerror!void {
        switch (self) {
            .Finite => |columns| {
                if (columns.len > 0) {
                    try columns[0].dumpInto(out_stream);
                    for (columns[1..]) |scalar_type| {
                        try out_stream.writeAll(" . ");
                        try scalar_type.dumpInto(out_stream);
                    }
                }
            },
            .Abstract => |type_of| {
                try type_of.dumpInto(out_stream);
            },
        }
    }
};
