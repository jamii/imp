usingnamespace @import("./common.zig");

const value = @import("./value.zig");

// ascii, non-empty
pub const Name = []const u8;

pub const BoxId = u64;

pub const Native = struct {
    name: Name,
    input_arity: usize,
    output_arity: usize,
    fun: fn([]const Scalar) NativeError ! value.Set,
};

pub const NativeError = error {
    // TODO
};

pub const Surface = union(enum) {
    None,
    Some,
    Scalar: value.Scalar,
    Union: Pair,
    Intersect: Pair,
    Product: Pair,
    Equal: Pair,
    Name: Name,
    When: When,
    Abstract: Abstract,
    Apply: Pair,
    Box: *const Surface,
    Annotate: Annotate,

    Negate: *const Surface,
    If: If,
    Let: Let,

    // Struct: Struct,
    // Lookup: Lookup,

    pub const Pair = struct {
        left: *const Surface,
        right: *const Surface,
    };

    pub const When = struct {
        condition: *const Surface,
        true_branch: *const Surface,
    };

    pub const Abstract = struct {
        args: []const Arg,
        body: *const Surface,
    };

    pub const Arg = struct {
        name: Name,
        unbox: bool,
    };

    pub const Annotate = struct {
        annotation: *const Surface,
        body: *const Surface,
    };

    pub const If = struct {
        condition: *const Surface,
        true_branch: *const Surface,
        false_branch: *const Surface,
    };

    pub const Let = struct {
        name: Name,
        value: *const Surface,
        body: *const Surface,
    };
};

pub const Core = union(enum) {
    None,
    Some,
    Scalar: value.Scalar,
    Union: Pair,
    Intersect: Pair,
    Product: Pair,
    Equal: Pair,
    Name: NameIx,
    Native: Native,
    When: Pair,
    Abstract: Abstract,
    Apply: Pair,
    Native: Native,
    Box: Box,
    Unbox: *const Core,
    Annotate: Annotate,

    pub const Pair = struct {
        left: *const Core,
        right: *const Core,
    };

    pub const NameIx = usize;

    pub const Abstract = struct {
        unbox: bool,
        body: *const Core,
    };

    pub const Box = struct {
        id: BoxId,
        scope: []const NameIx,
        body: *const Core,
    };

    pub const Annotate = struct {
        annotation: *const Core,
        body: *const Core,
    };
};
