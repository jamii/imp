const imp = @import("../../../imp.zig");
usingnamespace imp.common;
const meta = imp.meta;
const Store = imp.lang.Store;
const value = imp.lang.repr.value;
const core = imp.lang.repr.core2;

pub const ProgramType = struct {
    expr_type_unions: []const ArrayList(Specialization),
    program_type: SetType,

    pub fn dumpInto(self: ProgramType, writer: anytype, indent: u32) WriterError(@TypeOf(writer))!void {
        for (self.expr_type_unions) |expr_type_union, i| {
            if (i != 0) {
                try writer.writeAll("\n");
                try writer.writeByteNTimes(' ', indent);
            }
            try std.fmt.format(writer, "S{}", .{i});
            for (expr_type_union.items) |specialization| {
                try writer.writeAll("\n");
                try writer.writeByteNTimes(' ', indent + 4);
                try specialization.dumpInto(writer, indent + 4);
            }
        }
        try writer.writeAll("\n");
        try writer.writeByteNTimes(' ', indent);
        try self.program_type.dumpInto(writer, indent);
    }
};

pub const Specialization = struct {
    hint: []const ScalarType,
    set_type: SetType,

    pub fn dumpInto(self: Specialization, writer: anytype, indent: u32) WriterError(@TypeOf(writer))!void {
        try writer.writeAll("(");
        for (self.hint) |scalar_type| {
            try writer.writeAll(", ");
            try scalar_type.dumpInto(writer, indent);
        }
        try writer.writeAll(") ");
        try self.set_type.dumpInto(writer, indent);
    }
};

pub const SetType = union(enum) {
    /// The empty set
    None,
    /// A set with these column types
    Concrete: ConcreteSetType,

    pub fn isFinite(self: SetType) bool {
        return self == .None or (self == .Concrete and self.Concrete.abstract_arity == 0);
    }

    pub fn dumpInto(self: SetType, writer: anytype, indent: u32) WriterError(@TypeOf(writer))!void {
        switch (self) {
            .None => try writer.writeAll("none"),
            .Concrete => |concrete| {
                try concrete.dumpInto(writer, indent);
            },
        }
    }

    pub fn format(self: SetType, comptime fmt: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        // TODO https://github.com/ziglang/zig/issues/9220
        _ = fmt;
        try self.dumpInto(writer, 0);
    }
};

pub const ConcreteSetType = struct {
    abstract_arity: usize,
    columns: []const ScalarType,

    pub fn dumpInto(self: ConcreteSetType, writer: anytype, indent: u32) WriterError(@TypeOf(writer))!void {
        if (self.columns.len == 0) {
            try writer.writeAll("maybe");
        } else {
            for (self.columns) |scalar_type, scalar_type_ix| {
                if (scalar_type_ix < self.abstract_arity) {
                    try writer.writeAll("?");
                    try scalar_type.dumpInto(writer, indent);
                    try writer.writeAll(" ");
                } else {
                    if (scalar_type_ix != self.abstract_arity) try writer.writeAll(", ");
                    try scalar_type.dumpInto(writer, indent);
                }
            }
        }
    }

    pub fn format(self: ConcreteSetType, comptime fmt: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        // TODO https://github.com/ziglang/zig/issues/9220
        _ = fmt;
        try self.dumpInto(writer, 0);
    }
};

pub const ScalarType = union(enum) {
    Text,
    Number,
    Box: BoxType,

    pub fn dumpInto(self: ScalarType, writer: anytype, indent: u32) WriterError(@TypeOf(writer))!void {
        switch (self) {
            .Text => try writer.writeAll("text"),
            .Number => try writer.writeAll("number"),
            .Box => |box_type| {
                try writer.writeAll("@");
                try box_type.dumpInto(writer, indent);
            },
        }
    }

    pub fn format(self: ScalarType, comptime fmt: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        // TODO https://github.com/ziglang/zig/issues/9220
        _ = fmt;
        try self.dumpInto(writer, 0);
    }
};

pub const BoxType = union(enum) {
    Normal: struct {
        set_id: core.SetId,
        args: []ScalarType,
    },
    // While analyzing fix or reduce, need to temporarily assume a type to avoid infinite recursion
    FixOrReduce: struct {
        // The id of the `next` part of the fix/reduce expr, only used to check that this box type does not escape
        set_id: core.SetId,
        set_type: SetType,
    },

    pub fn dumpInto(self: BoxType, writer: anytype, indent: u32) WriterError(@TypeOf(writer))!void {
        switch (self) {
            .Normal => |normal| {
                try std.fmt.format(writer, "(S{}", .{normal.set_id});
                for (normal.args) |arg| {
                    try writer.writeAll(" ");
                    try arg.dumpInto(writer, indent);
                }
                try writer.writeAll(")");
            },
            .FixOrReduce => |fix_or_reduce| {
                try std.fmt.format(writer, "(#S{} ", .{fix_or_reduce.set_id});
                try fix_or_reduce.set_type.dumpInto(writer, indent);
                try writer.writeAll(")");
            },
        }
    }

    pub fn format(self: ScalarType, comptime fmt: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        // TODO https://github.com/ziglang/zig/issues/9220
        _ = fmt;
        try self.dumpInto(writer, 0);
    }
};
