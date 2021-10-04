const imp = @import("../imp.zig");
const meta = imp.meta;

pub const std = @import("std");
pub const builtin = @import("builtin");
pub const warn = std.debug.warn;
pub const assert = std.debug.assert;
pub const expect = std.testing.expect;
pub const max = std.math.max;
pub const min = std.math.min;
pub const Allocator = std.mem.Allocator;
pub const ArenaAllocator = std.heap.ArenaAllocator;
pub const ArrayList = std.ArrayList;

pub fn imp_panic(comptime fmt: []const u8, args: anytype) noreturn {
    const message = formatToString(std.heap.c_allocator, fmt, args) catch |err| message: {
        switch (err) {
            error.OutOfMemory => break :message "OOM inside panic",
        }
    };
    @panic(message);
}

pub fn TODO() noreturn {
    imp_panic("TODO", .{});
}

pub fn DeepHashMap(comptime K: type, comptime V: type) type {
    return std.HashMap(K, V, meta.DeepHashContext(K), std.hash_map.DefaultMaxLoadPercentage);
}

pub fn DeepHashSet(comptime K: type) type {
    return DeepHashMap(K, void);
}

pub fn formatToString(allocator: *Allocator, comptime fmt: []const u8, args: anytype) ![]const u8 {
    var buf = ArrayList(u8).init(allocator);
    var writer = buf.writer();
    try std.fmt.format(writer, fmt, args);
    return buf.items;
}

pub fn FixedSizeArrayList(comptime size: usize, comptime T: type) type {
    return struct {
        elems: [size]T = undefined,
        len: usize = 0,

        const Self = @This();

        pub fn append(self: *Self, elem: T) void {
            assert(self.len < size);
            self.elems[self.len] = elem;
            self.len += 1;
        }

        pub fn slice(self: *Self) []T {
            return self.elems[0..self.len];
        }
    };
}

pub fn Id(comptime dump_tag: []const u8) type {
    return struct {
        id: usize,
        const Self = @This();
        pub fn dumpInto(self: Self, writer: anytype, _: u32) WriterError(@TypeOf(writer))!void {
            try std.fmt.format(writer, "{s}{}", .{ dump_tag, self.id });
        }
        pub const format = formatViaDump;
    };
}

// This is only for debugging
pub fn dump(thing: anytype) void {
    const held = std.debug.getStderrMutex().acquire();
    defer held.release();
    const my_stderr = std.io.getStdErr().writer();
    imp.debug.dumpInto(my_stderr, 0, thing) catch return;
    my_stderr.writeAll("\n") catch return;
}

pub fn WriterError(comptime Writer: type) type {
    return @typeInfo(std.meta.declarationInfo(Writer, "writeAll").data.Fn.return_type).ErrorUnion.error_set;
}

// This is a sneaky global allocator, so that we can eg sort sets before printing them
pub const dump_allocator = std.heap.c_allocator;

pub fn dumpInto(writer: anytype, indent: u32, thing: anytype) WriterError(@TypeOf(writer))!void {
    const T = @TypeOf(thing);
    const ti = @typeInfo(T);
    switch (ti) {
        .Struct, .Enum, .Union => {
            if (@hasDecl(T, "dumpInto")) {
                return thing.dumpInto(writer, indent);
            }
        },
        else => {},
    }
    switch (ti) {
        .Pointer => |pti| {
            switch (pti.size) {
                .One => {
                    try writer.writeAll("&");
                    try dumpInto(writer, indent, thing.*);
                },
                .Many => {
                    // bail
                    try std.fmt.format(writer, "{any}", .{thing});
                },
                .Slice => {
                    if (pti.child == u8) {
                        try std.fmt.format(writer, "\"{s}\"", .{thing});
                    } else {
                        try std.fmt.format(writer, "[]{s}[\n", .{pti.child});
                        for (thing) |elem| {
                            try writer.writeByteNTimes(' ', indent + 4);
                            try dumpInto(writer, indent + 4, elem);
                            try writer.writeAll(",\n");
                        }
                        try writer.writeByteNTimes(' ', indent);
                        try writer.writeAll("]");
                    }
                },
                .C => {
                    // bail
                    try std.fmt.format(writer, "{any}", .{thing});
                },
            }
        },
        .Array => |ati| {
            if (ati.child == u8) {
                try std.fmt.format(writer, "\"{s}\"", .{thing});
            } else {
                try std.fmt.format(writer, "[{}]{s}[\n", .{ ati.len, ati.child });
                for (thing) |elem| {
                    try writer.writeByteNTimes(' ', indent + 4);
                    try dumpInto(writer, indent + 4, elem);
                    try writer.writeAll(",\n");
                }
                try writer.writeByteNTimes(' ', indent);
                try writer.writeAll("]");
            }
        },
        .Struct => |sti| {
            try writer.writeAll(@typeName(@TypeOf(thing)));
            try writer.writeAll("{\n");
            inline for (sti.fields) |field| {
                try writer.writeByteNTimes(' ', indent + 4);
                try std.fmt.format(writer, ".{s} = ", .{field.name});
                try dumpInto(writer, indent + 4, @field(thing, field.name));
                try writer.writeAll(",\n");
            }
            try writer.writeByteNTimes(' ', indent);
            try writer.writeAll("}");
        },
        .Union => |uti| {
            if (uti.tag_type) |tag_type| {
                try writer.writeAll(@typeName(@TypeOf(thing)));
                try writer.writeAll("{\n");
                inline for (@typeInfo(tag_type).Enum.fields) |fti| {
                    if (@enumToInt(std.meta.activeTag(thing)) == fti.value) {
                        try writer.writeByteNTimes(' ', indent + 4);
                        try std.fmt.format(writer, ".{s} = ", .{fti.name});
                        try dumpInto(writer, indent + 4, @field(thing, fti.name));
                        try writer.writeAll("\n");
                        try writer.writeByteNTimes(' ', indent);
                        try writer.writeAll("}");
                    }
                }
            } else {
                // bail
                try std.fmt.format(writer, "{any}", .{thing});
            }
        },
        .Optional => {
            if (thing == null) {
                try writer.writeAll("null");
            } else {
                try dumpInto(writer, indent, thing.?);
            }
        },
        else => {
            // bail
            try std.fmt.format(writer, "{any}", .{thing});
        },
    }
}

pub fn formatViaDump(self: anytype, comptime fmt: []const u8, _: std.fmt.FormatOptions, writer: anytype) WriterError(@TypeOf(writer))!void {
    // TODO https://github.com/ziglang/zig/issues/9220
    _ = fmt;
    try dumpInto(writer, 0, self.*);
}
