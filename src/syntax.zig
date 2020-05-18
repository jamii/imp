const common = import("./common.zig");
const value = import("./value.zig");

// ascii, non-empty
pub const Name = []const u8;

pub const BoxId = u64;

pub const Native = struct {
    name: Name,
    input_arity: usize,
    output_arity: usize,
    fun: fn([]const value.Scalar) -> NativeError ! value.Set,
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
    Name: Name,
    When: When,
    Abstract: Abstract,
    Apply: Pair,
    Box: *const Expr,
    Annotate: Annotate,

    Negate: *const Expr,
    If: If,
    Let: Let,
    Lookup: Lookup,

    fn apply(fun_expr: Expr, args: []Expr) Expr {
        var expr = fun_expr;
        for (args) |arg| {
            expr = Expr.{.Apply = .{.left=expr, .right=arg}};
        }
        return expr;
    }

    fn apply_name(name: Name, args: []Expr) Expr {
        return Expr.apply(Expr.{.Name=name}, args);
    }
};

pub const Pair = struct {
    left: *const Expr,
    right: *const Expr,
};

pub const When = struct {
    condition: *const Expr,
    true_branch: *const Expr,
};

pub const Abstract = struct {
    args: []const Arg,
    body: *const Expr,
};

pub const Arg = struct {
    name: Name,
    unbox: bool,
};

pub const Annotate = struct {
    annotation: Name,
    body: *const Expr,
};

pub const If = struct {
    condition: *const Expr,
    true_branch: *const Expr,
    false_branch: *const Expr,
};

pub const Let = struct {
    name: Name,
    value: *const Expr,
    body: *const Expr,
};

pub const Lookup = struct {
    value: *const Expr,
    name: Name,
}
