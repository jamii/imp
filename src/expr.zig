const common = import("./common.zig");
const value = import("./value.zig");

pub const Name = []const u8;

pub const Native = struct {
    name: Name,
    input_arity: usize,
    output_arity: usize,
    fun: fn([]const Scalar) -> NativeError ! value.Set,
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
    When: Pair,
    Abstract: Abstract,
    Apply: Pair,
    Native: Native,
    Seal: *const Surface,
    Unseal: *const Surface,
    Annotate: Annotate,

    Negate: *const Surface,
    If: If,
    Block: Block,
    Lookup: Lookup,

    pub const Pair = struct {
        left: *const Surface,
        right: *const Surface,
    };

    pub const Abstract = struct {
        name: Name,
        body: *const Surface,
    };

    pub const Annotate = struct {
        annotation: str,
        body: *const Surface,
    };

    pub const If = struct {
        condition: *const Surface,
        true_branch: *const Surface,
        false_branch: *const Surface,
    };

    pub const Block = struct {
        lets: []const Let,
        body: ?*const Surface,
    };

    pub const Let = struct {
        name: Name,
        value: *const Surface,
    };

    pub const Lookup = struct {
        value: *const Surface,
        name: Name,
    }
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
    When: Pair,
    Abstract: Abstract,
    Apply: Pair,
    Native: Native,
    Seal: Seal,
    Unseal: *const Core,
    Annotate: Annotate,

    pub const NameIx = usize;

    pub const Pair = struct {
        left: *const Core,
        right: *const Core,
    };

    pub const Abstract = struct {
        name: Name,
        body: *const Core,
    };

    pub const Seal = struct {
        id: u64,
        scope: []const NameIx,
        body: *const Core,
    };

    pub const Annotate = struct {
        annotation: str,
        body: *const Core,
    };
};
