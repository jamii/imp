usingnamespace @import("./common.zig");
const core = @import("./core.zig");

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

    // Equality on expr pointer and scope value

    pub fn deepHashInto(hasher: var, self: Box) void {
        hasher.update(std.mem.asBytes(&@ptrToInt(self.expr)));
        meta.deepHashInto(hasher, self.scope);
    }

    pub fn deepEqual(self: Box, other: Box) bool {
        return self.expr == other.expr and meta.deepEqual(self.scope, other.scope);
    }
};

pub const Tuple = []const Scalar;

pub const FiniteSet = DeepHashSet(Tuple);

pub const AbstractSet = struct {
    scope: Tuple,
    body: *const core.Expr,
};

pub const Set = union(enum) {
    Finite: FiniteSet,
    Abstract: AbstractSet,

    pub fn isFinite(self: Set) bool {
        return std.meta.activeTag(self) == .Finite;
    }
};
