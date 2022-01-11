const std = @import("std");
const builtin = @import("builtin");
const imp = @import("../imp.zig");

pub const warn = std.log.warn;
pub const assert = std.debug.assert;
pub const expect = std.testing.expect;
pub const max = std.math.max;
pub const min = std.math.min;
pub const Allocator = std.mem.Allocator;
pub const ArenaAllocator = std.heap.ArenaAllocator;
pub const ArrayList = std.ArrayList;

pub fn panic(comptime fmt: []const u8, args: anytype) noreturn {
    const message = formatToString(std.heap.c_allocator, fmt, args) catch |err| message: {
        switch (err) {
            error.OutOfMemory => break :message "OOM inside panic",
        }
    };
    @panic(message);
}

pub fn TODO() noreturn {
    panic("TODO", .{});
}

pub fn formatToString(allocator: Allocator, comptime fmt: []const u8, args: anytype) ![]const u8 {
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
    const stderr_mutex = std.debug.getStderrMutex();
    stderr_mutex.lock();
    defer stderr_mutex.unlock();
    const my_stderr = std.io.getStdErr().writer();
    dumpInto(my_stderr, 0, thing) catch return;
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

pub const Ordering = enum {
    LessThan,
    Equal,
    GreaterThan,
};

pub fn deepEqual(a: anytype, b: @TypeOf(a)) bool {
    return deepCompare(a, b) == .Equal;
}

// TODO This is needed for comparing/hashing DeepHashSet, but it's gross and slow.
//      Would be better to use a sorted set instead of DeepHashSet
const sorting_allocator = std.heap.c_allocator;
fn sortHashMap(hash_map: anytype) []const @TypeOf(hash_map).Entry {
    var elems = ArrayList(@TypeOf(hash_map).Entry).init(sorting_allocator);
    var iter = hash_map.iterator();
    while (iter.next()) |elem| elems.append(elem) catch panic("OOM", .{});
    deepSort(elems.items);
    return elems.toOwnedSlice();
}

pub fn deepCompare(a: anytype, b: @TypeOf(a)) Ordering {
    const T = @TypeOf(a);
    const ti = @typeInfo(T);
    if (comptime std.mem.startsWith(u8, @typeName(@TypeOf(a)), "std.hash_map.HashMap")) {
        const a_elems = sortHashMap(a);
        defer sorting_allocator.free(a_elems);
        const b_elems = sortHashMap(b);
        defer sorting_allocator.free(b_elems);
        return deepCompare(a_elems, b_elems);
    }
    switch (ti) {
        .Struct, .Enum, .Union => {
            if (@hasDecl(T, "overrideDeepCompare")) {
                return T.overrideDeepCompare(a, b);
            }
        },
        else => {},
    }
    switch (ti) {
        .Bool => {
            if (a == b) return .Equal;
            if (a) return .GreaterThan;
            return .LessThan;
        },
        .Int, .Float => {
            if (a < b) {
                return .LessThan;
            }
            if (a > b) {
                return .GreaterThan;
            }
            return .Equal;
        },
        .Enum => {
            return deepCompare(@enumToInt(a), @enumToInt(b));
        },
        .Pointer => |pti| {
            switch (pti.size) {
                .One => {
                    return deepCompare(a.*, b.*);
                },
                .Slice => {
                    if (a.len < b.len) {
                        return .LessThan;
                    }
                    if (a.len > b.len) {
                        return .GreaterThan;
                    }
                    for (a) |a_elem, a_ix| {
                        const ordering = deepCompare(a_elem, b[a_ix]);
                        if (ordering != .Equal) {
                            return ordering;
                        }
                    }
                    return .Equal;
                },
                .Many, .C => @compileError("cannot deepCompare " ++ @typeName(T)),
            }
        },
        .Optional => {
            if (a) |a_val| {
                if (b) |b_val| {
                    return deepCompare(a_val, b_val);
                } else {
                    return .GreaterThan;
                }
            } else {
                if (b) |_| {
                    return .LessThan;
                } else {
                    return .Equal;
                }
            }
        },
        .Array => {
            for (a) |a_elem, a_ix| {
                const ordering = deepCompare(a_elem, b[a_ix]);
                if (ordering != .Equal) {
                    return ordering;
                }
            }
            return .Equal;
        },
        .Struct => |sti| {
            inline for (sti.fields) |fti| {
                const ordering = deepCompare(@field(a, fti.name), @field(b, fti.name));
                if (ordering != .Equal) {
                    return ordering;
                }
            }
            return .Equal;
        },
        .Union => |uti| {
            if (uti.tag_type) |tag_type| {
                const enum_info = @typeInfo(tag_type).Enum;
                const a_tag = @enumToInt(@as(tag_type, a));
                const b_tag = @enumToInt(@as(tag_type, b));
                if (a_tag < b_tag) {
                    return .LessThan;
                }
                if (a_tag > b_tag) {
                    return .GreaterThan;
                }
                inline for (enum_info.fields) |fti| {
                    if (a_tag == fti.value) {
                        return deepCompare(
                            @field(a, fti.name),
                            @field(b, fti.name),
                        );
                    }
                }
                unreachable;
            } else {
                @compileError("cannot deepCompare " ++ @typeName(T));
            }
        },
        .Void => return .Equal,
        .ErrorUnion => {
            if (a) |a_ok| {
                if (b) |b_ok| {
                    return deepCompare(a_ok, b_ok);
                } else |_| {
                    return .LessThan;
                }
            } else |a_err| {
                if (b) |_| {
                    return .GreaterThan;
                } else |b_err| {
                    return deepCompare(a_err, b_err);
                }
            }
        },
        .ErrorSet => return deepCompare(@errorToInt(a), @errorToInt(b)),
        else => @compileError("cannot deepCompare " ++ @typeName(T)),
    }
}

pub fn deepSort(slice: anytype) void {
    const T = @typeInfo(@TypeOf(slice)).Pointer.child;
    std.sort.sort(T, slice, {}, struct {
        fn lessThan(_: void, a: T, b: T) bool {
            return deepCompare(a, b) == .LessThan;
        }
    }.lessThan);
}

pub fn deepHash(key: anytype) u64 {
    var hasher = std.hash.Wyhash.init(0);
    deepHashInto(&hasher, key);
    return hasher.final();
}

pub fn deepHashInto(hasher: anytype, key: anytype) void {
    const T = @TypeOf(key);
    const ti = @typeInfo(T);
    if (comptime std.mem.startsWith(u8, @typeName(T), "std.hash_map.HashMap")) {
        const elems = sortHashMap(key);
        defer sorting_allocator.free(elems);
        deepHashInto(hasher, elems);
        return;
    }
    switch (ti) {
        .Struct, .Enum, .Union => {
            if (@hasDecl(T, "overrideDeepHashInto")) {
                return T.overrideDeepHashInto(hasher, key);
            }
        },
        else => {},
    }
    switch (ti) {
        .Int => @call(.{ .modifier = .always_inline }, hasher.update, .{std.mem.asBytes(&key)}),
        .Float => |info| deepHashInto(hasher, @bitCast(std.meta.Int(.unsigned, info.bits), key)),
        .Bool => deepHashInto(hasher, @boolToInt(key)),
        .Enum => deepHashInto(hasher, @enumToInt(key)),
        .Pointer => |pti| {
            switch (pti.size) {
                .One => deepHashInto(hasher, key.*),
                .Slice => {
                    for (key) |element| {
                        deepHashInto(hasher, element);
                    }
                },
                .Many, .C => @compileError("cannot deepHash " ++ @typeName(T)),
            }
        },
        .Optional => if (key) |k| deepHashInto(hasher, k),
        .Array => {
            for (key) |element| {
                deepHashInto(hasher, element);
            }
        },
        .Struct => |info| {
            inline for (info.fields) |field| {
                deepHashInto(hasher, @field(key, field.name));
            }
        },
        .Union => |info| {
            if (info.tag_type) |tag_type| {
                const enum_info = @typeInfo(tag_type).Enum;
                const tag = std.meta.activeTag(key);
                deepHashInto(hasher, tag);
                inline for (enum_info.fields) |enum_field| {
                    if (enum_field.value == @enumToInt(tag)) {
                        deepHashInto(hasher, @field(key, enum_field.name));
                        return;
                    }
                }
                unreachable;
            } else @compileError("cannot deepHash " ++ @typeName(T));
        },
        .Void => {},
        else => @compileError("cannot deepHash " ++ @typeName(T)),
    }
}

pub fn DeepHashContext(comptime K: type) type {
    return struct {
        const Self = @This();
        pub fn hash(_: Self, pseudo_key: K) u64 {
            return deepHash(pseudo_key);
        }
        pub fn eql(_: Self, pseudo_key: K, key: K) bool {
            return deepEqual(pseudo_key, key);
        }
    };
}

pub fn DeepHashMap(comptime K: type, comptime V: type) type {
    return std.HashMap(K, V, DeepHashContext(K), std.hash_map.default_max_load_percentage);
}

pub fn DeepHashSet(comptime K: type) type {
    return DeepHashMap(K, void);
}
