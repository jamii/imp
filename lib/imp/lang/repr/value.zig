const imp = @import("../../../imp.zig");
usingnamespace imp.common;
const meta = imp.meta;
const Store = imp.lang.Store;
const core = imp.lang.repr.core;

pub const Scalar = union(enum) {
    Text: []const u8, // valid utf8
    Number: f64,
    Box: Box,

    fn dumpInto(self: Scalar, out_stream: var) anyerror!void {
        switch (self) {
            .Text => |text| try std.fmt.format(out_stream, "\"{s}\"", .{text}),
            .Number => |number| try std.fmt.format(out_stream, "{d}", .{number}),
            .Box => |box| {
                // TODO figure out a better way to name these
                try std.fmt.format(out_stream, "[box #{};", .{Store.getCoreMeta(box.expr).id});
                for (box.scope) |scalar, i| {
                    try out_stream.writeAll(if (i == 0) " " else " . ");
                    try scalar.dumpInto(out_stream);
                }
                try out_stream.writeAll("]");
            }
        }
    }
};

pub const Box = struct {
    expr: *const core.Expr,
    scope: Tuple,

    // Equality on expr id and scope value

    pub fn deepHashInto(hasher: var, self: Box) void {
        hasher.update(std.mem.asBytes(&Store.getCoreMeta(self.expr).id));
        meta.deepHashInto(hasher, self.scope);
    }

    pub fn deepCompare(self: Box, other: Box) meta.Ordering {
        const ordering = meta.deepCompare(Store.getCoreMeta(self.expr).id, Store.getCoreMeta(other.expr).id);
        if (ordering != .Equal) return ordering;
        return meta.deepCompare(self.scope, other.scope);
    }
};

pub const Tuple = []const Scalar;

pub const FiniteSet = DeepHashSet(Tuple);

/// Invariant: LazySet may not contain `some`
pub const LazySet = union (enum) {
    Abstract: LazyAbstract,
    /// Invariant: left is lazy, right is finite, arity(left) > arity(right)
    Apply: LazyPair,
    Union: LazyPair,
    Intersect: LazyPair,
    Product: LazyPair,
};

pub const LazyAbstract = struct {
    abstract: *const core.Expr,
    scope: Tuple,

    // Equality on expr id and scope value

    pub fn deepHashInto(hasher: var, self: LazyAbstract) void {
        hasher.update(std.mem.asBytes(&Store.getCoreMeta(self.abstract).id));
        meta.deepHashInto(hasher, self.scope);
    }

    pub fn deepCompare(self: LazyAbstract, other: LazyAbstract) meta.Ordering {
        const ordering = meta.deepCompare(Store.getCoreMeta(self.abstract).id, Store.getCoreMeta(other.abstract).id);
        if (ordering != .Equal) return ordering;
        return meta.deepCompare(self.scope, other.scope);
    }
};

pub const LazyPair = struct {
    left: *Set,
    right: *Set,
};

/// Invariant: all the tuples in a Set must be the same length
pub const Set = union(enum) {
    Finite: FiniteSet,
    Lazy: LazySet,

    // TODO this is not an ordering, only works for deepEqual
    pub fn deepCompare(self: Set, other: Set) meta.Ordering {
        if (self == .Finite and other == .Finite) {
            if (self.Finite.count() != other.Finite.count()) {
                return .LessThan;
            }
            var self_iter = self.Finite.iterator();
            while (self_iter.next()) |kv| {
                if (!other.Finite.contains(kv.key)) {
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
            .Finite => |finite| {
                var tuples = ArrayList(Tuple).init(allocator);
                defer tuples.deinit();
                var iter = finite.iterator();
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
            },
            .Lazy => |lazy| {
                try out_stream.writeAll("<lazy>");
            },
        }
    }
};
