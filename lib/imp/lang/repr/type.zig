const std = @import("std");
const imp = @import("../../../imp.zig");
const u = imp.util;
const value = imp.lang.repr.value;
const core = imp.lang.repr.core;

pub const ProgramType = struct {
    defs: []const u.ArrayList(Specialization),
    program_type: SetType,
    warnings: []const Warning,

    pub fn dumpInto(self: ProgramType, writer: anytype, indent: u32) u.WriterError(@TypeOf(writer))!void {
        for (self.defs) |def, i| {
            if (i != 0) {
                try writer.writeAll("\n");
                try writer.writeByteNTimes(' ', indent);
            }
            try std.fmt.format(writer, "{}", .{i});
            for (def.items) |specialization| {
                try writer.writeAll("\n");
                try writer.writeByteNTimes(' ', indent + 4);
                try specialization.dumpInto(writer, indent + 4);
            }
        }
        try writer.writeAll("\n");
        try writer.writeByteNTimes(' ', indent);
        try self.program_type.dumpInto(writer, indent);
        if (self.warnings.len > 0) {
            try writer.writeAll("\n");
            try writer.writeByteNTimes(' ', indent);
            try writer.writeAll("warnings:");
            for (self.warnings) |warning| {
                try writer.writeAll("\n");
                try writer.writeByteNTimes(' ', indent + 4);
                try warning.dumpInto(writer, indent);
            }
        }
    }
};

pub const Specialization = struct {
    hint: []const ScalarType,
    hint_mode: HintMode,
    set_type: SetType,

    pub fn dumpInto(self: Specialization, writer: anytype, indent: u32) u.WriterError(@TypeOf(writer))!void {
        try writer.writeAll("(");
        try std.fmt.format(writer, "{} ", .{self.hint_mode});
        for (self.hint) |scalar_type| {
            try writer.writeAll(", ");
            try scalar_type.dumpInto(writer, indent);
        }
        try writer.writeAll(") ");
        try self.set_type.dumpInto(writer, indent);
    }
};

pub const HintMode = enum {
    Apply,
    Intersect,
};

pub const SetType = union(enum) {
    row_types: u.DeepHashSet(RowType),

    pub fn none(
        allocator: u.Allocator,
    ) SetType {
        var row_types = u.DeepHashSet(RowType).init(allocator);
        return SetType{ .row_types = row_types };
    }

    pub fn some(
        allocator: u.Allocator,
    ) !SetType {
        var row_types = u.DeepHashSet(RowType).init(allocator);
        try row_types.put(.{ .columns = &.{} }, {});
        return SetType{ .row_types = row_types };
    }

    pub fn fromScalar(allocator: u.Allocator, scalar_type: ScalarType) !SetType {
        var row_types = u.DeepHashSet(RowType).init(allocator);
        const columns = [_]ScalarType{scalar_type};
        try row_types.put(.{ .columns = try allocator.dupe(ScalarType, &columns) }, {});
        return SetType{ .row_types = row_types };
    }

    pub fn fromColumns(allocator: u.Allocator, columns: []const ScalarType) !SetType {
        var row_types = u.DeepHashSet(RowType).init(allocator);
        try row_types.put(.{ .columns = try allocator.dupe(ScalarType, columns) }, {});
        return SetType{ .row_types = row_types };
    }

    pub fn isBoolish(self: SetType) bool {
        var row_types_iter = self.row_types.keyIterator();
        while (row_types_iter.next()) |row_type| {
            if (row_type.columns.len > 0) return false;
        }
        return true;
    }

    pub fn isFinite(self: SetType) bool {
        var row_types_iter = self.row_types.keyIterator();
        while (row_types_iter.next()) |row_type| {
            if (!row_type.isFinite()) return false;
        }
        return true;
    }

    pub fn dumpInto(self: SetType, writer: anytype, indent: u32) u.WriterError(@TypeOf(writer))!void {
        if (self.row_types.count() == 0) {
            try writer.writeAll("none");
        } else {
            var row_types_iter = self.row_types.keyIterator();
            var is_first = true;
            while (row_types_iter.next()) |row_type| {
                if (!is_first) try writer.writeAll(" | ");
                is_first = false;
                try row_type.dumpInto(writer, indent);
            }
        }
    }

    pub const format = u.formatViaDump;
};

pub const RowType = struct {
    columns: []const ScalarType,

    pub fn dumpInto(self: RowType, writer: anytype, indent: u32) u.WriterError(@TypeOf(writer))!void {
        if (self.columns.len == 0) {
            try writer.writeAll("maybe");
        } else {
            for (self.columns) |column_type, ix| {
                if (ix > 0) try writer.writeAll(", ");
                try column_type.dumpInto(writer, indent);
            }
        }
    }

    pub const format = u.formatViaDump;
};

pub const ScalarType = union(enum) {
    Text,
    Number,
    Box: BoxType,
    TextTag: []const u8, // valid utf8
    NumberTag: f64,

    pub fn isTag(self: ScalarType) bool {
        return switch (self) {
            .Number, .Text, .Box => false,
            .NumberTag, .TextTag => true,
        };
    }

    pub fn dumpInto(self: ScalarType, writer: anytype, indent: u32) u.WriterError(@TypeOf(writer))!void {
        switch (self) {
            .Text => try writer.writeAll("text"),
            .Number => try writer.writeAll("number"),
            .Box => |box_type| {
                try writer.writeAll("@");
                try box_type.dumpInto(writer, indent);
            },
            .TextTag => |text| try (value.Scalar{ .TextTag = text }).dumpInto(writer, indent),
            .NumberTag => |number| try (value.Scalar{ .NumberTag = number }).dumpInto(writer, indent),
        }
    }

    pub const format = u.formatViaDump;
};

pub const BoxType = union(enum) {
    Normal: struct {
        def_id: core.DefId,
        args: []ScalarType,
    },
    // While analyzing fix or reduce, need to temporarily assume a type to avoid infinite recursion
    FixOrReduce: struct {
        // The id of the `next` part of the fix/reduce expr, only used to check that this box type does not escape
        def_id: core.DefId,
        set_type: SetType,
    },

    pub fn dumpInto(self: BoxType, writer: anytype, indent: u32) u.WriterError(@TypeOf(writer))!void {
        switch (self) {
            .Normal => |normal| {
                try std.fmt.format(writer, "({}", .{normal.def_id});
                for (normal.args) |arg| {
                    try writer.writeAll(" ");
                    try arg.dumpInto(writer, indent);
                }
                try writer.writeAll(")");
            },
            .FixOrReduce => |fix_or_reduce| {
                try std.fmt.format(writer, "(#{} ", .{fix_or_reduce.def_id});
                try fix_or_reduce.set_type.dumpInto(writer, indent);
                try writer.writeAll(")");
            },
        }
    }

    pub const format = u.formatViaDump;
};

pub const Warning = struct {
    expr_id: core.ExprId,
    message: []const u8,

    pub fn dumpInto(self: Warning, writer: anytype, indent: u32) u.WriterError(@TypeOf(writer))!void {
        _ = indent;
        try writer.writeAll(self.message);
    }
};
