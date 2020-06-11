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
        return self == .Concrete and self.Concrete.abstract_arity == 0;
    }

    pub fn dumpInto(self: SetType, out_stream: var) error{OutOfMemory}!void {
        switch (self) {
            .Concrete => |concrete| {
                try concrete.dumpInto(out_stream);
            },
            .Lazy => |lazy| {
                try lazy.dumpInto(out_stream);
            },
        }
    }

    pub fn format(self: SetType, comptime fmt: []const u8, options: std.fmt.FormatOptions, out_stream: var) !void {
        try self.dumpInto(out_stream);
    }
};

pub const ConcreteSetType = struct {
    abstract_arity: usize,
    columns: []ScalarType,

    pub fn dumpInto(self: ConcreteSetType, out_stream: var) error{OutOfMemory}!void {
        if (self.columns.len == 0) {
            try out_stream.writeAll("maybe");
        } else {
            for (self.columns) |scalar_type, i| {
                try out_stream.writeAll(
                    if (i == 0) ""
                        else if (i <= self.abstract_arity) " -> "
                        else " . ");
                try scalar_type.dumpInto(out_stream);
            }
        }
    }

    pub fn format(self: ConcreteSetType, comptime fmt: []const u8, options: std.fmt.FormatOptions, out_stream: var) !void {
        try self.dumpInto(out_stream);
    }
};

pub const LazySetType = struct {
    expr: *const core.Expr,
    scope: []const ScalarType,

    // Equality on expr id and scope value

    pub fn deepHashInto(hasher: var, self: LazySetType) void {
        hasher.update(std.mem.asBytes(&@ptrToInt(self.expr)));
        meta.deepHashInto(hasher, self.scope);
    }

    pub fn deepCompare(self: LazySetType, other: LazySetType) meta.Ordering {
        const ordering = meta.deepCompare(Store.getCoreMeta(self.expr).id, Store.getCoreMeta(other.expr).id);
        if (ordering != .Equal) return ordering;
        return meta.deepCompare(self.scope, other.scope);
    }

    pub fn dumpInto(self: LazySetType, out_stream: var) error{OutOfMemory}!void {
        try std.fmt.format(out_stream, "(type of expr {} with scope", .{Store.getCoreMeta(self.expr).id});
        for (self.scope) |scalar_type| {
            try out_stream.writeAll(" ");
            try scalar_type.dumpInto(out_stream);
        }
        try out_stream.writeAll(")");
    }

    pub fn format(self: LazySetType, comptime fmt: []const u8, options: std.fmt.FormatOptions, out_stream: var) !void {
        try self.dumpInto(out_stream);
    }
};

pub const ScalarType = union(enum) {
    Text,
    Number,
    Box: LazySetType,

    pub fn dumpInto(self: ScalarType, out_stream: var) error{OutOfMemory}!void {
        switch (self) {
            .Text => try out_stream.writeAll("text"),
            .Number => try out_stream.writeAll("number"),
            .Box => |set_type| {
                try out_stream.writeAll("[");
                try set_type.dumpInto(out_stream);
                try out_stream.writeAll("]");
            },
        }
    }

    pub fn format(self: ScalarType, comptime fmt: []const u8, options: std.fmt.FormatOptions, out_stream: var) !void {
        try self.dumpInto(out_stream);
    }
};
