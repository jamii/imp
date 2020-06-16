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

    pub fn dumpInto(self: SetType, out_stream: var) OutStreamError(@TypeOf(out_stream))!void {
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

    pub fn dumpInto(self: ConcreteSetType, out_stream: var) OutStreamError(@TypeOf(out_stream))!void {
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
    time: []const TimeType,

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

    pub fn dumpInto(self: LazySetType, out_stream: var) OutStreamError(@TypeOf(out_stream))!void {
        try std.fmt.format(out_stream, "(type of expr #{} with scope (", .{Store.getCoreMeta(self.expr).id});
        for (self.scope) |scalar_type, i| {
            try out_stream.writeAll(if (i == 0) "" else " . ");
            try scalar_type.dumpInto(out_stream);
        }
        try out_stream.writeAll("))");
    }

    pub fn format(self: LazySetType, comptime fmt: []const u8, options: std.fmt.FormatOptions, out_stream: var) !void {
        try self.dumpInto(out_stream);
    }
};

pub const TimeType = enum {
    Iteration,

    // this is here because can't use slices on zero-sized types
    Unused,
};

pub const ScalarType = union(enum) {
    Text,
    Number,
    Box: BoxType,

    pub fn dumpInto(self: ScalarType, out_stream: var) OutStreamError(@TypeOf(out_stream))!void {
        switch (self) {
            .Text => try out_stream.writeAll("text"),
            .Number => try out_stream.writeAll("number"),
            .Box => |box_type| {
                try out_stream.writeAll("[");
                try box_type.dumpInto(out_stream);
                try out_stream.writeAll("]");
            },
        }
    }

    pub fn format(self: ScalarType, comptime fmt: []const u8, options: std.fmt.FormatOptions, out_stream: var) !void {
        try self.dumpInto(out_stream);
    }
};

pub const BoxType = struct {
    // always need a lazy type because box types are nominal - determined by the expr itself
    lazy: LazySetType,
    // need to store the concrete type when analyzing fixpoints to avoid infinite recursion
    concrete: ?ConcreteSetType,

    pub fn dumpInto(self: BoxType, out_stream: var) OutStreamError(@TypeOf(out_stream))!void {
        try self.lazy.dumpInto(out_stream);
    }

    pub fn format(self: ScalarType, comptime fmt: []const u8, options: std.fmt.FormatOptions, out_stream: var) !void {
        try self.dumpInto(out_stream);
    }
};
