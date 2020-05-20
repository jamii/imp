usingnamespace @import("./common.zig");

const value = @import("./value.zig");

// ascii, non-empty
pub const Name = []const u8;

pub const BoxId = u64;

pub const Native = struct {
    name: Name,
    input_arity: usize,
    output_arity: usize,
    fun: fn([]const value.Scalar) NativeError ! value.Set,
};

pub const NativeError = error {
    // TODO
};

pub const Expr = union(enum) {
    None,
    Some,
    Scalar: value.Scalar,
    Union: Pair,
    Intersect: Pair,
    Product: Pair,
    Equal: Pair,
    Name: NameIx,
    When: Pair,
    Abstract: Abstract,
    Apply: Pair,
    Box: Box,
    Annotate: Annotate,

    Native: Native,
};

pub const Pair = struct {
    left: *const Expr,
    right: *const Expr,
};

pub const NameIx = usize;

pub const Abstract = struct {
    unbox: bool,
    body: *const Expr,
};

pub const Box = struct {
    id: BoxId,
    scope: []const NameIx,
    body: *const Expr,
};

pub const Annotate = struct {
    annotation: *const Expr,
    body: *const Expr,
};
