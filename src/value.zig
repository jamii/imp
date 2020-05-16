usingnamespace @import("./common.zig");
const expr = @import("./expr.zig");

pub const Scalar = union(enum) {
    String: []const u8,
    Number: f64,
    Seal: *Seal,
};

pub const Seal = struct {
    id: u64,
    scope: Tuple,
    // value: Set,

    pub fn deepEqual(a: Seal, b: Seal) bool {
        return (a.id == b.id) and meta.deepEqual(a.scope, b.scope);
        // don't compare key.value
    }

    pub fn deepHashInto(hasher: var, key: Seal) void {
        meta.deepHashInto(hasher, key.id);
        meta.deepHashInto(hasher, key.scope);
        // don't hash key.value
    }
};

pub const Tuple = []const Scalar;

pub const Finite = DeepHashSet(Tuple);

pub const Abstract = struct {
    scope: Tuple,
    body: *const expr.Core,
};

pub const Set = union(enum) {
    Finite: Finite,
    Abstract: Abstract,
};

// TODO kinda pointless test, but exercises lots of code
test "seal equality" {
    const a_scope = [2]Scalar{.{.Number = 42}, .{.String = "foo"}};
    const a = Seal {
        .id = 0,
        .scope = &a_scope,
    };

    const b_scope = [2]Scalar{.{.Number = 42}, .{.String = "foo"}};
    const b = Seal {
        .id = 0,
        .scope = &b_scope,
    };

    const c_scope = [2]Scalar{.{.Number = 42}, .{.String = "foo"}};
    const c = Seal {
        .id = 1,
        .scope = &c_scope,
    };

    const d_scope = [2]Scalar{.{.Number = 1}, .{.String = "blah"}};
    const d = Seal {
        .id = 0,
        .scope = &d_scope,
    };

    // same id/scope, different value
    expect(meta.deepEqual(a,b));

    // different id
    expect(!meta.deepEqual(a,c));

    // different scope
    expect(!meta.deepEqual(a,d));
}
