usingnamespace @import("./common.zig");
const expr = @import("./expr.zig");

pub const Scalar = union(enum) {
    String: []const u8, // valid utf8
    Number: f64,
    Box: Box,
};

pub const Box = struct {
    id: expr.BoxId,
    scope: Tuple,
};

pub const Tuple = []const Scalar;

pub const FiniteSet = DeepHashSet(Tuple);

pub const AbstractSet = struct {
    scope: Tuple,
    body: *const expr.Core,
};

pub const Set = union(enum) {
    Finite: FiniteSet,
    Abstract: AbstractSet,
};
