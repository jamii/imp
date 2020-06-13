const imp = @import("../../../imp.zig");
usingnamespace imp.common;
const meta = imp.meta;
const Store = imp.lang.Store;
const core = imp.lang.repr.core;

/// Invariant: all the tuples in a Set must be the same length
pub const Set = union(enum) {
    Finite: FiniteSet,
    Lazy: LazySet,

    // TODO this is not an ordering, only works for deepEqual
    pub fn deepCompare(self: Set, other: Set) meta.Ordering {
        if (self == .Finite and other == .Finite) {
            if (self.Finite.set.count() != other.Finite.set.count()) {
                return .LessThan;
            }
            var self_iter = self.Finite.set.iterator();
            while (self_iter.next()) |kv| {
                if (!other.Finite.set.contains(kv.key)) {
                    return .LessThan;
                }
            }
            return .Equal;
        }
        if (self == .Lazy and other == .Lazy) {
            return meta.deepCompare(self.Lazy, other.Lazy);
        }
        return .LessThan;
    }

    pub fn dumpInto(self: Set, allocator: *Allocator, out_stream: var) anyerror!void {
        switch(self) {
            .Finite => |finite| try finite.dumpInto(allocator, out_stream),
            .Lazy => |lazy| try lazy.dumpInto(out_stream),
        }
    }
};

pub const Tuple = []const Scalar;

pub const FiniteSet = struct {
    arity: usize,
    set: DeepHashSet(Tuple),


    pub fn dumpInto(self: FiniteSet, allocator: *Allocator, out_stream: var) anyerror!void {
        var tuples = ArrayList(Tuple).init(allocator);
        defer tuples.deinit();
        var iter = self.set.iterator();
        while (iter.next()) |kv| {
            try tuples.append(kv.key);
        }
        std.sort.sort(Tuple, tuples.items, struct {
            fn lessThan(a: Tuple, b: Tuple) bool {
                return meta.deepCompare(a,b) == .LessThan;
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
                    try out_stream.writeAll(if (j == 0) "" else " . ");
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

    fn dumpInto(self: Scalar, out_stream: var) anyerror!void {
        switch (self) {
            // TODO proper escaping
            .Text => |text| try std.fmt.format(out_stream, "\"{s}\"", .{text}),
            .Number => |number| try std.fmt.format(out_stream, "{d}", .{number}),
            .Box => |box| {
                try out_stream.writeAll("[");
                try box.dumpInto(out_stream);
                try out_stream.writeAll("]");
            }
        }
    }
};

pub const Time = usize;

pub const LazySet = struct {
    expr: *const core.Expr,
    scope: Tuple,
    time: []const Time,

    // Equality on expr id and scope value

    pub fn deepHashInto(hasher: var, self: LazySet) void {
        hasher.update(std.mem.asBytes(&Store.getCoreMeta(self.expr).id));
        meta.deepHashInto(hasher, self.scope);
    }

    pub fn deepCompare(self: LazySet, other: LazySet) meta.Ordering {
        const ordering = meta.deepCompare(Store.getCoreMeta(self.expr).id, Store.getCoreMeta(other.expr).id);
        if (ordering != .Equal) return ordering;
        return meta.deepCompare(self.scope, other.scope);
    }

    pub fn dumpInto(self: LazySet, out_stream: var) anyerror ! void {
        try std.fmt.format(out_stream, "(value of expr #{} with scope (", .{Store.getCoreMeta(self.expr).id});
        for (self.scope) |scalar| {
            try out_stream.writeAll(" ");
            try scalar.dumpInto(out_stream);
        }
        try out_stream.writeAll("))");
    }
};
