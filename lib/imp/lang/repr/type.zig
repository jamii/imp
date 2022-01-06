const std = @import("std");
const imp = @import("../../../imp.zig");
const u = imp.util;
const value = imp.lang.repr.value;
const core = imp.lang.repr.core;

pub const ProgramType = struct {
    defs: []const u.ArrayList(Specialization),
    program_type: SetType,

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
    }
};

pub const Specialization = struct {
    hint: []const ScalarType,
    set_type: SetType,

    pub fn dumpInto(self: Specialization, writer: anytype, indent: u32) u.WriterError(@TypeOf(writer))!void {
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
    concretes: u.DeepHashSet(ConcreteSetType),

    pub fn none(
        allocator: u.Allocator,
    ) SetType {
        var concretes = u.DeepHashSet(ConcreteSetType).init(allocator);
        return SetType{ .concretes = concretes };
    }

    pub fn some(
        allocator: u.Allocator,
    ) !SetType {
        var concretes = u.DeepHashSet(ConcreteSetType).init(allocator);
        try concretes.put(.{ .columns = &.{} }, {});
        return SetType{ .concretes = concretes };
    }

    pub fn fromScalar(allocator: u.Allocator, scalar_type: ScalarType) !SetType {
        var concretes = u.DeepHashSet(ConcreteSetType).init(allocator);
        const columns = [_]ScalarType{scalar_type};
        try concretes.put(.{ .columns = try allocator.dupe(ScalarType, &columns) }, {});
        return SetType{ .concretes = concretes };
    }

    pub fn fromColumns(allocator: u.Allocator, columns: []const ScalarType) !SetType {
        var concretes = u.DeepHashSet(ConcreteSetType).init(allocator);
        try concretes.put(.{ .columns = try allocator.dupe(ScalarType, columns) }, {});
        return SetType{ .concretes = concretes };
    }

    pub fn isBoolish(self: SetType) bool {
        var concretes_iter = self.concretes.keyIterator();
        while (concretes_iter.next()) |concrete| {
            if (concrete.columns.len > 0) return false;
        }
        return true;
    }

    pub fn isFinite(self: SetType) bool {
        var concretes_iter = self.concretes.keyIterator();
        while (concretes_iter.next()) |concrete| {
            if (!concrete.isFinite()) return false;
        }
        return true;
    }

    pub fn dumpInto(self: SetType, writer: anytype, indent: u32) u.WriterError(@TypeOf(writer))!void {
        if (self.concretes.count() == 0) {
            try writer.writeAll("none");
        } else {
            var concretes_iter = self.concretes.keyIterator();
            var is_first = true;
            while (concretes_iter.next()) |concrete| {
                if (!is_first) try writer.writeAll(" | ");
                is_first = false;
                try concrete.dumpInto(writer, indent);
            }
        }
    }

    pub const format = u.formatViaDump;
};

pub const ConcreteSetType = struct {
    columns: []const ScalarType,

    pub fn dumpInto(self: ConcreteSetType, writer: anytype, indent: u32) u.WriterError(@TypeOf(writer))!void {
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
    StagedText: []const u8, // valid utf8
    StagedNumber: f64,

    pub fn isStaged(self: ScalarType) bool {
        return switch (self) {
            .Number, .Text, .Box => false,
            .StagedNumber, .StagedText => true,
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
            .StagedText => |text| try (value.Scalar{ .StagedText = text }).dumpInto(writer, indent),
            .StagedNumber => |number| try (value.Scalar{ .StagedNumber = number }).dumpInto(writer, indent),
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
