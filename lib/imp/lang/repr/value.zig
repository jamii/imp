const imp = @import("../../../imp.zig");
usingnamespace imp.common;
const meta = imp.meta;
const Store = imp.lang.Store;
const core = imp.lang.repr.core;

/// Invariant: all the tuples in a Set must be the same length
pub const Set = union(enum) {
    Finite: FiniteSet,
    Lazy: LazySet,

    pub fn dumpInto(self: Set, allocator: *Allocator, out_stream: anytype) OutStreamError(@TypeOf(out_stream))!void {
        switch (self) {
            .Finite => |finite| try finite.dumpInto(allocator, out_stream),
            .Lazy => |lazy| try lazy.dumpInto(out_stream),
        }
    }

    pub fn format(self: Set, comptime fmt: []const u8, options: std.fmt.FormatOptions, out_stream: anytype) !void {
        switch (self) {
            .Finite => |finite| try finite.format(fmt, options, out_stream),
            .Lazy => |lazy| try lazy.dumpInto(out_stream),
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
            if (!other.set.contains(kv.key)) {
                return .LessThan;
            }
        }
        return .Equal;
    }

    pub fn dumpInto(self: FiniteSet, allocator: *Allocator, out_stream: anytype) OutStreamError(@TypeOf(out_stream))!void {
        var tuples = ArrayList(Tuple).init(allocator);
        defer tuples.deinit();
        var iter = self.set.iterator();
        while (iter.next()) |kv| {
            // TODO errors for printing are funky
            tuples.append(kv.key) catch imp_panic("oom", .{});
        }
        std.sort.sort(Tuple, tuples.items, {}, struct {
            fn lessThan(_: void, a: Tuple, b: Tuple) bool {
                return meta.deepCompare(a, b) == .LessThan;
            }
        }.lessThan);
        if (tuples.items.len == 0) {
            try out_stream.writeAll("none");
        } else if (tuples.items.len == 1 and tuples.items[0].len == 0) {
            try out_stream.writeAll("some");
        } else {
            for (tuples.items) |tuple, i| {
                try out_stream.writeAll(if (i == 0) "" else " | ");
                for (tuple) |scalar, j| {
                    try out_stream.writeAll(if (j == 0) "" else " , ");
                    try scalar.dumpInto(out_stream);
                }
            }
        }
    }

    pub fn format(self: FiniteSet, comptime fmt: []const u8, options: std.fmt.FormatOptions, out_stream: anytype) !void {
        if (self.set.count() == 0) {
            try out_stream.writeAll("none");
        } else if (self.set.count() == 1 and self.set.iterator().next().?.key.len == 0) {
            try out_stream.writeAll("some");
        } else {
            var iter = self.set.iterator();
            var i: usize = 0;
            while (iter.next()) |kv| : (i += 1) {
                try out_stream.writeAll(if (i == 0) "" else " | ");
                for (kv.key) |scalar, j| {
                    try out_stream.writeAll(if (j == 0) "" else " , ");
                    try scalar.dumpInto(out_stream);
                }
            }
        }
    }
};

pub const Scalar = union(enum) {
    Text: []const u8, // valid utf8
    Number: f64,
    Box: LazySet,

    fn dumpInto(self: Scalar, out_stream: anytype) OutStreamError(@TypeOf(out_stream))!void {
        switch (self) {
            // TODO proper escaping
            .Text => |text| try std.fmt.format(out_stream, "\"{s}\"", .{text}),
            .Number => |number| try std.fmt.format(out_stream, "{d}", .{number}),
            .Box => |box| {
                try out_stream.writeAll("[");
                try box.dumpInto(out_stream);
                try out_stream.writeAll("]");
            },
        }
    }

    pub fn format(self: Scalar, comptime fmt: []const u8, options: std.fmt.FormatOptions, out_stream: anytype) !void {
        try self.dumpInto(out_stream);
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

    pub fn dumpInto(self: LazySet, out_stream: anytype) OutStreamError(@TypeOf(out_stream))!void {
        try std.fmt.format(out_stream, "(value of expr #{} with scope (", .{Store.getCoreMeta(self.expr).id});
        for (self.scope) |scalar| {
            try out_stream.writeAll(" ");
            try scalar.dumpInto(out_stream);
        }
        try out_stream.writeAll("))");
    }
};
