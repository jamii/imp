pub const std = @import("std");
pub const warn = std.debug.warn;
pub const assert = std.debug.assert;
pub const expect = std.testing.expect;
pub const panic = std.debug.panic;
pub const max = std.math.max;
pub const min = std.math.min;
pub const Allocator = std.mem.Allocator;
pub const ArenaAllocator = std.heap.ArenaAllocator;
pub const ArrayList = std.ArrayList;

pub const meta = @import("./meta.zig");

pub fn DeepHashMap(comptime K: type, comptime V: type) type {
    return std.HashMap(
        K, V,
        struct {
            fn hash(key: K) u32 {
                return meta.deepHash(key);
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
    dumpInner(my_stderr.*, 0, thing) catch return;
    my_stderr.writeAll("\n") catch return;
}

fn dumpInner(out_stream: var, indent: u32, thing: var) !void {
    const ti = @typeInfo(@TypeOf(thing));
    switch (ti) {
        .Struct => |sti| {
            try out_stream.writeAll(@typeName(@TypeOf(thing)));
            try out_stream.writeAll("{\n");
            inline for (sti.fields) |field| {
                try out_stream.writeByteNTimes(' ', indent + 4);
                try std.fmt.format(out_stream, ".{} = ", .{field.name});
                try dumpInner(out_stream, indent + 4, @field(thing, field.name));
                try out_stream.writeAll(",\n");
            }
            try out_stream.writeByteNTimes(' ', indent);
            try out_stream.writeAll("}");
        },
        .Array => |ati| {
            if (ati.child == u8) {
                try std.fmt.format(out_stream, "\"{s}\"", .{thing});
            } else {
                try std.fmt.format(out_stream, "[{}]{}[\n", .{ ati.len, @typeName(ati.child) });
                for (thing) |elem| {
                    try out_stream.writeByteNTimes(' ', indent + 4);
                    try dumpInner(out_stream, indent + 4, elem);
                    try out_stream.writeAll(",\n");
                }
                try out_stream.writeByteNTimes(' ', indent);
                try out_stream.writeAll("]");
            }
        },
        .Pointer => |pti| {
            switch (pti.size) {
                .One => {
                    try out_stream.writeAll("&");
                    try dumpInner(out_stream, indent, thing.*);
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
                            try dumpInner(out_stream, indent + 4, elem);
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
        else => {
            // bail
            try std.fmt.format(out_stream, "{}", .{thing});
        },
    }
}

pub fn format(allocator: *Allocator, comptime fmt: str, args: var) !str {
    var buf = ArrayList(u8).init(allocator);
    var out = buf.outStream();
    try std.fmt.format(out, fmt, args);
    return buf.items;
}
