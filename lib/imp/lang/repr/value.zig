const imp = @import("../../../imp.zig");
usingnamespace imp.common;
const meta = imp.meta;
const Store = imp.lang.Store;
const core = imp.lang.repr.core;

/// Invariant: all the tuples in a Set must be the same length
pub const Set = union(enum) {
    Finite: FiniteSet,
    Lazy: LazySet,

    pub fn dumpInto(self: Set, allocator: *Allocator, writer: anytype) WriterError(@TypeOf(writer))!void {
        switch (self) {
            .Finite => |finite| try finite.dumpInto(allocator, writer),
            .Lazy => |lazy| try lazy.dumpInto(writer),
        }
    }

    pub fn format(self: Set, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        // TODO https://github.com/ziglang/zig/issues/9220
        _ = fmt;
        switch (self) {
            .Finite => |finite| try finite.format(fmt, options, writer),
            .Lazy => |lazy| try lazy.dumpInto(writer),
        }
    }
};

pub const Tuple = []const Scalar;

pub const FiniteSet = struct {
    arity: usize,
    set: DeepHashSet(Tuple),

    // TODO this is not an ordering, only works for deepEqual
    pub fn deepCompare(self: FiniteSet, other: FiniteSet) meta.Ordering {
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

    pub fn deepHashInto(hasher: anytype, self: FiniteSet) void {
        meta.deepHashInto(hasher, self.arity);
        // TODO this will break if hashmap starts using a random seed
        var iter = self.set.iterator();
        while (iter.next()) |entry| {
            meta.deepHashInto(hasher, entry.key_ptr.*);
        }
    }

    pub fn dumpInto(self: FiniteSet, allocator: *Allocator, writer: anytype) WriterError(@TypeOf(writer))!void {
        var tuples = ArrayList(Tuple).init(allocator);
        defer tuples.deinit();
        var iter = self.set.iterator();
        while (iter.next()) |kv| {
            // TODO errors for printing are funky
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
            for (tuples.items) |tuple| {
                try writer.writeAll("| ");
                for (tuple) |scalar, scalar_ix| {
                    if (scalar_ix != 0) try writer.writeAll(", ");
                    try scalar.dumpInto(writer);
                }
                try writer.writeAll("\n");
            }
        }
    }

    pub fn format(self: FiniteSet, comptime fmt: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        // TODO https://github.com/ziglang/zig/issues/9220
        _ = fmt;
        if (self.set.count() == 0) {
            try writer.writeAll("none");
        } else if (self.set.count() == 1 and self.set.iterator().next().?.key_ptr.len == 0) {
            try writer.writeAll("some");
        } else {
            var iter = self.set.iterator();
            var i: usize = 0;
            while (iter.next()) |kv| : (i += 1) {
                try writer.writeAll("| ");
                for (kv.key_ptr.*) |scalar, scalar_ix| {
                    if (scalar_ix != 0) try writer.writeAll(", ");
                    try scalar.dumpInto(writer);
                }
                try writer.writeAll("\n");
            }
        }
    }
};

pub const Scalar = union(enum) {
    Text: []const u8, // valid utf8
    Number: f64,
    Box: LazySet,

    pub fn dumpInto(self: Scalar, writer: anytype) WriterError(@TypeOf(writer))!void {
        switch (self) {
            // TODO proper escaping
            .Text => |text| try std.fmt.format(writer, "\"{s}\"", .{text}),
            .Number => |number| try std.fmt.format(writer, "{d}", .{number}),
            .Box => |box| {
                try writer.writeAll("@");
                try box.dumpInto(writer);
            },
        }
    }

    pub fn format(self: Scalar, comptime fmt: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        // TODO https://github.com/ziglang/zig/issues/9220
        _ = fmt;
        try self.dumpInto(writer);
    }
};

pub const Time = usize;

pub const LazySet = struct {
    expr: *const core.Expr,
    scope: Tuple,
    time: []const Time,

    // Equality on expr id and scope/time value

    pub fn deepHashInto(hasher: anytype, self: LazySet) void {
        hasher.update(std.mem.asBytes(&Store.getCoreMeta(self.expr).id));
        meta.deepHashInto(hasher, self.scope);
        meta.deepHashInto(hasher, self.time);
    }

    pub fn deepCompare(self: LazySet, other: LazySet) meta.Ordering {
        const id_ordering = meta.deepCompare(Store.getCoreMeta(self.expr).id, Store.getCoreMeta(other.expr).id);
        if (id_ordering != .Equal) return id_ordering;
        const scope_ordering = meta.deepCompare(self.scope, other.scope);
        if (scope_ordering != .Equal) return scope_ordering;
        return meta.deepCompare(self.time, other.time);
    }

    pub fn dumpInto(self: LazySet, writer: anytype) WriterError(@TypeOf(writer))!void {
        try std.fmt.format(writer, "(value of expr #{} with scope (", .{Store.getCoreMeta(self.expr).id});
        for (self.scope) |scalar| {
            // TODO want to print boxes once we have reasonable scoping
            if (scalar != .Box) {
                try scalar.dumpInto(writer);
                try writer.writeAll(", ");
            }
        }
        try writer.writeAll("))");
    }
};
