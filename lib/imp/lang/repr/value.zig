const imp = @import("../../../imp.zig");
usingnamespace imp.common;
const meta = imp.meta;
const core = imp.lang.repr.core;

/// Invariant: all the Tuples in a Set must be the same length
pub const Set = struct {
    arity: usize,
    set: DeepHashSet(Tuple),

    // TODO this is not an ordering, only works for deepEqual
    pub fn deepCompare(self: Set, other: Set) meta.Ordering {
        if (self.set.count() != other.set.count()) {
            return .LessThan;
        }
        var self_iter = self.set.iterator();
        while (self_iter.next()) |kv| {
            if (!other.set.contains(kv.key_ptr.*)) {
                return .LessThan;
            }
        }
        return .Equal;
    }

    pub fn deepHashInto(hasher: anytype, self: Set) void {
        meta.deepHashInto(hasher, self.arity);
        // TODO this will break if hashmap starts using a random seed
        var iter = self.set.iterator();
        while (iter.next()) |entry| {
            meta.deepHashInto(hasher, entry.key_ptr.*);
        }
    }

    pub fn dumpInto(self: Set, writer: anytype, indent: u32) WriterError(@TypeOf(writer))!void {
        var tuples = ArrayList(Tuple).init(dump_allocator);
        defer tuples.deinit();
        var iter = self.set.iterator();
        while (iter.next()) |kv| {
            // TODO format doesn't always allow OOM
            tuples.append(kv.key_ptr.*) catch imp_panic("oom", .{});
        }
        std.sort.sort(Tuple, tuples.items, {}, struct {
            fn lessThan(_: void, a: Tuple, b: Tuple) bool {
                return meta.deepCompare(a, b) == .LessThan;
            }
        }.lessThan);
        if (tuples.items.len == 0) {
            try writer.writeAll("none\n");
        } else if (tuples.items.len == 1 and tuples.items[0].len == 0) {
            try writer.writeAll("some\n");
        } else {
            for (tuples.items) |tuple, i| {
                if (i != 0) {
                    try writer.writeAll("\n");
                    try writer.writeByteNTimes(' ', indent);
                }
                try writer.writeAll("| ");
                for (tuple) |scalar, scalar_ix| {
                    if (scalar_ix != 0) try writer.writeAll(", ");
                    try scalar.dumpInto(writer, indent);
                }
            }
        }
    }

    pub const format = formatViaDump;
};

pub const Tuple = []const Scalar;

pub const Scalar = union(enum) {
    Text: []const u8, // valid utf8
    Number: f64,
    Box: Box,

    pub fn dumpInto(self: Scalar, writer: anytype, indent: u32) WriterError(@TypeOf(writer))!void {
        switch (self) {
            // TODO proper escaping
            .Text => |text| try std.fmt.format(writer, "\"{s}\"", .{text}),
            .Number => |number| try std.fmt.format(writer, "{d}", .{number}),
            .Box => |box| {
                try writer.writeAll("@");
                try box.dumpInto(writer, indent);
            },
        }
    }

    pub const format = formatViaDump;
};

pub const Box = union(enum) {
    Normal: struct {
        def_id: core.DefId,
        args: []const Scalar,
    },
    // While interpreting fix or reduce, need to pass an actual value to avoid infinite recursion
    // TODO *const c_void is actually a *const Set - workaround for https://github.com/ziglang/zig/issues/5920
    FixOrReduce: *align(@alignOf(Set)) const c_void,

    pub fn deepCompare(self: Box, other: Box) meta.Ordering {
        const tagOrdering = meta.deepCompare(std.meta.activeTag(self), std.meta.activeTag(other));
        if (tagOrdering != .Equal) return tagOrdering;
        switch (self) {
            .Normal => return meta.deepCompare(self.Normal, other.Normal),
            .FixOrReduce => return meta.deepCompare(self.getFixOrReduce(), other.getFixOrReduce()),
        }
    }

    pub fn deepHashInto(hasher: anytype, self: Box) void {
        meta.deepHashInto(hasher, std.meta.activeTag(self));
        switch (self) {
            .Normal => |normal| meta.deepHashInto(hasher, normal),
            .FixOrReduce => meta.deepHashInto(hasher, self.getFixOrReduce()),
        }
    }

    pub fn fixOrReduce(allocator: *Allocator, set: Set) !Box {
        const set_ptr = try allocator.create(Set);
        set_ptr.* = set;
        return Box{ .FixOrReduce = @ptrCast(*align(@alignOf(Set)) const c_void, set_ptr) };
    }

    pub fn getFixOrReduce(box: Box) Set {
        const set_ptr = @ptrCast(*const Set, box.FixOrReduce);
        return set_ptr.*;
    }

    pub fn dumpInto(self: Box, writer: anytype, indent: u32) WriterError(@TypeOf(writer))!void {
        switch (self) {
            .Normal => |normal| {
                try std.fmt.format(writer, "({}", .{normal.def_id});
                for (normal.args) |arg| {
                    try writer.writeAll(" ");
                    try arg.dumpInto(writer, indent);
                }
                try writer.writeAll(")");
            },
            .FixOrReduce => {
                try writer.writeAll("(");
                try self.getFixOrReduce().dumpInto(writer, indent);
                try writer.writeAll(")");
            },
        }
    }

    pub const format = formatViaDump;
};
