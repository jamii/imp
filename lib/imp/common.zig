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

pub fn imp_panic(comptime fmt: []const u8, args: var) noreturn {
    const message = format(std.heap.c_allocator, fmt, args) catch |err| message: {
        switch (err) {
            error.OutOfMemory => break :message "OOM inside panic",
        }
    };
    @panic(message);
}

pub fn DeepHashMap(comptime K: type, comptime V: type) type {
    return std.HashMap(
        K, V,
        struct {
            fn hash(key: K) u32 {
                return @truncate(u32, meta.deepHash(key));
            }
        }.hash,
        struct {
            fn equal(a: K, b: K) bool {
                return meta.deepEqual(a,b);
            }
        }.equal
    );
}

pub fn DeepHashSet(comptime K: type) type {
    return DeepHashMap(K, void);
}

pub fn dump(thing: var) void {
    const held = std.debug.getStderrMutex().acquire();
    defer held.release();
    const my_stderr = std.debug.getStderrStream();
    dumpInto(my_stderr.*, 0, thing) catch return;
    my_stderr.writeAll("\n") catch return;
}

pub fn dumpInto(out_stream: var, indent: u32, thing: var) anyerror!void {
        const ti = @typeInfo(@TypeOf(thing));
        switch (ti) {
            .Pointer => |pti| {
                switch (pti.size) {
                    .One => {
                        try out_stream.writeAll("&");
                        try dumpInto(out_stream, indent, thing.*);
                    },
                    .Many => {
                        // bail
                        try std.fmt.format(out_stream, "{}", .{thing});
                    },
                    .Slice => {
                        if (pti.child == u8) {
                            try std.fmt.format(out_stream, "\"{s}\"", .{thing});
                        } else {
                            try std.fmt.format(out_stream, "[]{}[\n", .{@typeName(pti.child)});
                            for (thing) |elem| {
                                try out_stream.writeByteNTimes(' ', indent + 4);
                                try dumpInto(out_stream, indent + 4, elem);
                                try out_stream.writeAll(",\n");
                            }
                            try out_stream.writeByteNTimes(' ', indent);
                            try out_stream.writeAll("]");
                        }
                    },
                    .C => {
                        // bail
                        try std.fmt.format(out_stream, "{}", .{thing});
                    },
                }
            },
            .Array => |ati| {
                if (ati.child == u8) {
                    try std.fmt.format(out_stream, "\"{s}\"", .{thing});
                } else {
                    try std.fmt.format(out_stream, "[{}]{}[\n", .{ ati.len, @typeName(ati.child) });
                    for (thing) |elem| {
                        try out_stream.writeByteNTimes(' ', indent + 4);
                        try dumpInto(out_stream, indent + 4, elem);
                        try out_stream.writeAll(",\n");
                    }
                    try out_stream.writeByteNTimes(' ', indent);
                    try out_stream.writeAll("]");
                }
            },
            .Struct => |sti| {
                try out_stream.writeAll(@typeName(@TypeOf(thing)));
                try out_stream.writeAll("{\n");
                inline for (sti.fields) |field| {
                    try out_stream.writeByteNTimes(' ', indent + 4);
                    try std.fmt.format(out_stream, ".{} = ", .{field.name});
                    try dumpInto(out_stream, indent + 4, @field(thing, field.name));
                    try out_stream.writeAll(",\n");
                }
                try out_stream.writeByteNTimes(' ', indent);
                try out_stream.writeAll("}");
            },
            .Union => |uti| {
                if (uti.tag_type) |tag_type| {
                    try out_stream.writeAll(@typeName(@TypeOf(thing)));
                    try out_stream.writeAll("{\n");
                    inline for (uti.fields) |fti| {
                        const field = fti.enum_field.?;
                        if (@enumToInt(std.meta.activeTag(thing)) == field.value) {
                            try out_stream.writeByteNTimes(' ', indent + 4);
                            try std.fmt.format(out_stream, ".{} = ", .{field.name});
                            try dumpInto(out_stream, indent + 4, @field(thing, field.name));
                            try out_stream.writeAll("\n");
                            try out_stream.writeByteNTimes(' ', indent);
                            try out_stream.writeAll("}");
                        }
                    }
                } else {
                    // bail
                    try std.fmt.format(out_stream, "{}", .{thing});
                }
            },
            else => {
                // bail
                try std.fmt.format(out_stream, "{}", .{thing});
            },
        }
}

pub fn format(allocator: *Allocator, comptime fmt: []const u8, args: var) ![]const u8 {
    var buf = ArrayList(u8).init(allocator);
    var out = buf.outStream();
    try std.fmt.format(out, fmt, args);
    return buf.items;
}

pub fn Result(comptime Ok: type, comptime Err: type) type {
    return union(enum) {
        Ok: Ok,
        Err: Err,
    };
}

pub fn tagEqual(a: var, b: @TypeOf(a)) bool {
    return std.meta.activeTag(a) == std.meta.activeTag(b);
}

pub fn FixedSizeArrayList(comptime size: usize, comptime T: type) type {
    return struct {
        elems: [size]T,
        len: usize,

        const Self = @This();

        pub fn init() Self {
            return Self{
                .elems = undefined,
                .len = 0,
            };
        }

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

pub fn TODO() noreturn {
    imp_panic("TODO", .{});
}
