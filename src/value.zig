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
                try std.fmt.format(out_stream, "[{} |", .{box.id});
                for (box.scope) |scalar| {
                    try scalar.dumpInto(out_stream);
                    try out_stream.writeAll(" ");
                }
                try out_stream.writeAll("]");
            }
        }
    }
};

pub const Box = struct {
    id: core.BoxId,
    scope: Tuple,
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
