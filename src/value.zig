usingnamespace @import("./common.zig");
const core = @import("./core.zig");
const type_ = @import("./type.zig");
const value = @import("./value.zig");

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
                try std.fmt.format(out_stream, "[{};", .{meta.deepHash(box)});
                if (box.scope.len > 0) {
                    try box.scope[0].dumpInto(out_stream);
                    for (box.scope[1..]) |scalar| {
                        try out_stream.writeAll(" . ");
                        try scalar.dumpInto(out_stream);
                    }
                }
                try out_stream.writeAll("]");
            }
        }
    }
};

pub const Box = struct {
    expr: *const core.Expr,
    scope: Tuple,
    // TODO this triggers a compiler bug :|
    // value: Set,

    // Equality on expr pointer and scope value

    pub fn deepHashInto(hasher: var, self: Box) void {
        hasher.update(std.mem.asBytes(&@ptrToInt(self.expr)));
        meta.deepHashInto(hasher, self.scope);
    }

    pub fn deepEqual(self: Box, other: Box) bool {
        return self.expr == other.expr
            and meta.deepEqual(self.scope, other.scope);
    }
};

pub const Tuple = []const Scalar;

pub const FiniteSet = DeepHashSet(Tuple);

pub const AbstractSet = struct {
    scope: Tuple,
    scope_types: []const type_.ScalarType,
    body: *const core.Expr,
};

pub const Set = union(enum) {
    Finite: FiniteSet,
    Abstract: AbstractSet,

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
                        // TODO make a meta.deepCompare
                        // TODO this won't be stable across runs for boxes
                        return meta.deepHash(a) < meta.deepHash(b);
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
            .Abstract => |abstract| {
                // TODO would be much nicer to just print value
                try std.fmt.format(out_stream, "[{};", .{meta.deepHash(abstract.body)});
                for (abstract.scope) |scalar, i| {
                    try out_stream.writeAll(if (i == 0) " " else " . ");
                    try scalar.dumpInto(out_stream);
                }
                try out_stream.writeAll("]");
            },
        }
    }
};
