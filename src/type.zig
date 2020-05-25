usingnamespace @import("./common.zig");

const value = @import("./value.zig");
const core = @import("./core.zig");

pub const ScalarType = union(enum) {
    Any,
    Box: SetType,

    pub fn union_(a: ScalarType, b: ScalarType) ScalarType {
        if (a == .Box and b == .Box and meta.deepEqual(a.Box, b.Box)) {
            return a;
        } else {
            return .Any;
        }
    }

    pub fn intersect(a: ScalarType, b: ScalarType) ScalarType {
        // TODO should unequal boxes really return Any?
        if (a == .Box and b == .Box and meta.deepEqual(a.Box, b.Box)) {
            return a;
        } else {
            return .Any;
        }
    }
};

/// Represents whatever the type of this expr with this scope is
pub const TypeOf = struct {
    expr: *const core.Expr,
    scope: []const ScalarType,

    // Equality on expr pointer and scope value

    pub fn deepHashInto(hasher: var, self: TypeOf) void {
        hasher.update(std.mem.asBytes(&@ptrToInt(self.expr)));
        meta.deepHashInto(hasher, self.scope);
    }

    pub fn deepEqual(self: TypeOf, other: TypeOf) bool {
        return self.expr == other.expr and meta.deepEqual(self.scope, other.scope);
    }
};

pub const SetType = union(enum) {
    /// A finite set with these column types
    Finite: []const ScalarType,
    /// Something that we can't type until it's specialized
    Abstract: TypeOf,
};
