const std = @import("std");
const imp = @import("../../../imp.zig");
const u = imp.util;
const core = imp.lang.repr.core;
const type_ = imp.lang.repr.type_;

/// Invariant: all the Rows in a Set must be the same length
pub const Set = struct {
    rows: u.DeepHashSet(Row),

    pub const Arity = union(enum) {
        Unknown,
        Known: usize,
        Mixed,
    };

    // TODO Set.none, Set.some, Set.fromScalar, Set.fromRow

    pub fn getArity(self: Set) Arity {
        var arity: Arity = .Unknown;
        {
            var iter = self.rows.iterator();
            while (iter.next()) |entry| {
                switch (arity) {
                    .Unknown => arity = .{ .Known = entry.key_ptr.len },
                    .Known => |known| if (known != entry.key_ptr.len) {
                        arity = .Mixed;
                    },
                    .Mixed => {},
                }
            }
        }
        return arity;
    }

    pub fn getType(self: Set, allocator: u.Allocator) !type_.SetType {
        var row_types = u.DeepHashSet(type_.RowType).init(allocator);
        var iter = self.rows.keyIterator();
        while (iter.next()) |row|
            try row_types.put(try getRowType(row.*, allocator), {});
        return type_.SetType{ .row_types = row_types };
    }

    // TODO this is not an ordering, only works for deepEqual
    pub fn overrideDeepCompare(self: Set, other: Set) u.Ordering {
        if (self.rows.count() != other.rows.count()) {
            return .LessThan;
        }
        var self_iter = self.rows.iterator();
        while (self_iter.next()) |kv| {
            if (!other.rows.contains(kv.key_ptr.*)) {
                return .LessThan;
            }
        }
        return .Equal;
    }

    pub fn overrideDeepHashInto(hasher: anytype, self: Set) void {
        // TODO this will break if hashmap starts using a random seed
        var iter = self.rows.iterator();
        while (iter.next()) |entry| {
            u.deepHashInto(hasher, entry.key_ptr.*);
        }
    }

    pub fn dumpInto(self: Set, writer: anytype, indent: u32) u.WriterError(@TypeOf(writer))!void {
        var rows = u.ArrayList(Row).init(u.dump_allocator);
        defer rows.deinit();
        var iter = self.rows.iterator();
        while (iter.next()) |kv| {
            // TODO format doesn't always allow OOM
            rows.append(kv.key_ptr.*) catch u.panic("oom", .{});
        }
        std.sort.sort(Row, rows.items, {}, struct {
            fn lessThan(_: void, a: Row, b: Row) bool {
                return u.deepCompare(a, b) == .LessThan;
            }
        }.lessThan);
        if (rows.items.len == 0) {
            try writer.writeAll("none");
        } else if (rows.items.len == 1 and rows.items[0].len == 0) {
            try writer.writeAll("some");
        } else {
            for (rows.items) |row, i| {
                if (i != 0) {
                    try writer.writeAll("\n");
                    try writer.writeByteNTimes(' ', indent);
                }
                try writer.writeAll("| ");
                for (row) |scalar, scalar_ix| {
                    if (scalar_ix != 0) try writer.writeAll(", ");
                    try scalar.dumpInto(writer, indent);
                }
            }
        }
    }

    pub const format = u.formatViaDump;
};

pub const Row = []const Scalar;

pub fn getRowType(self: Row, allocator: u.Allocator) !type_.RowType {
    var scalar_types = u.ArrayList(type_.ScalarType).init(allocator);
    for (self) |scalar|
        try scalar_types.append(try scalar.getType(allocator));
    return type_.RowType{ .scalar_types = scalar_types.toOwnedSlice() };
}

pub const Scalar = union(enum) {
    Text: []const u8, // valid utf8
    Number: f64,
    Box: Box,
    TextTag: []const u8, // valid utf8
    NumberTag: f64,

    pub fn getType(self: Scalar, allocator: u.Allocator) !type_.ScalarType {
        return switch (self) {
            .Text => .Text,
            .Number => .Number,
            .Box => |box| .{ .Box = try box.getType(allocator) },
            .TextTag => |text| .{ .TextTag = text },
            .NumberTag => |number| .{ .NumberTag = number },
        };
    }

    pub fn dumpInto(self: Scalar, writer: anytype, indent: u32) u.WriterError(@TypeOf(writer))!void {
        switch (self) {
            // TODO proper escaping
            .Text => |text| try std.fmt.format(writer, "\"{}\"", .{std.zig.fmtEscapes(text)}),
            .Number => |number| try std.fmt.format(writer, "{d}", .{number}),
            .Box => |box| {
                try writer.writeAll("@");
                try box.dumpInto(writer, indent);
            },
            .TextTag => |text| {
                // If text is valid name, print without quotes
                if (imp.lang.pass.parse.isName(text))
                    try std.fmt.format(writer, ":{s}", .{text})
                else
                    try std.fmt.format(writer, ":\"{}\"", .{std.zig.fmtEscapes(text)});
            },
            .NumberTag => |number| try std.fmt.format(writer, ":{d}", .{number}),
        }
    }

    pub const format = u.formatViaDump;
};

pub const Name = imp.lang.syntax.Name;

pub const Box = union(enum) {
    Normal: struct {
        def_id: core.DefId,
        args: []const Scalar,
    },
    // While interpreting fix or reduce, need to pass an actual value to avoid infinite recursion
    // TODO *const anyopaque is actually a *const Set - workaround for https://github.com/ziglang/zig/issues/5920
    FixOrReduce: *align(@alignOf(Set)) const anyopaque,

    pub fn getType(self: Box, allocator: u.Allocator) error{OutOfMemory}!type_.BoxType {
        switch (self) {
            .Normal => |normal| {
                const args = try allocator.alloc(type_.ScalarType, normal.args.len);
                for (args) |*arg, i|
                    arg.* = try normal.args[i].getType(allocator);
                return type_.BoxType{ .Normal = .{ .def_id = normal.def_id, .args = args } };
            },
            .FixOrReduce =>
            // We only use getType on constants, so this shouldn't be reachable
            u.panic("TODO: cannot infer type of Box.FixOrReduce", .{}),
        }
    }

    pub fn overrideDeepCompare(self: Box, other: Box) u.Ordering {
        const tagOrdering = u.deepCompare(std.meta.activeTag(self), std.meta.activeTag(other));
        if (tagOrdering != .Equal) return tagOrdering;
        switch (self) {
            .Normal => return u.deepCompare(self.Normal, other.Normal),
            .FixOrReduce => return u.deepCompare(self.getFixOrReduce(), other.getFixOrReduce()),
        }
    }

    pub fn overrideDeepHashInto(hasher: anytype, self: Box) void {
        u.deepHashInto(hasher, std.meta.activeTag(self));
        switch (self) {
            .Normal => |normal| u.deepHashInto(hasher, normal),
            .FixOrReduce => u.deepHashInto(hasher, self.getFixOrReduce()),
        }
    }

    pub fn fixOrReduce(allocator: u.Allocator, set: Set) !Box {
        const set_ptr = try allocator.create(Set);
        set_ptr.* = set;
        return Box{ .FixOrReduce = @ptrCast(*align(@alignOf(Set)) const anyopaque, set_ptr) };
    }

    pub fn getFixOrReduce(box: Box) Set {
        const set_ptr = @ptrCast(*const Set, box.FixOrReduce);
        return set_ptr.*;
    }

    pub fn dumpInto(self: Box, writer: anytype, indent: u32) u.WriterError(@TypeOf(writer))!void {
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

    pub const format = u.formatViaDump;
};
