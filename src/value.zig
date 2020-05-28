usingnamespace @import("./common.zig");
const core = @import("./core.zig");
const type_ = @import("./type.zig");
const Store = @import("./store.zig").Store;

pub const Scalar = union(enum) {
    String: []const u8, // valid utf8
    Number: f64,
    Box: Box,

    fn dumpInto(self: Scalar, out_stream: var) anyerror!void {
        switch (self) {
            .String => |string| try std.fmt.format(out_stream, "\"{s}\"", .{string}),
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
    body: *const core.Expr,
    scope: Tuple,
};

pub const LazyPair = struct {
    left: *Set,
    right: *Set,
};

/// Invariant: all the tuples in a Set must be the same length
pub const Set = union(enum) {
    Finite: FiniteSet,
    Lazy: LazySet,

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
                try out_stream.writeAll("(lazy)");
            },
        }
    }
};
