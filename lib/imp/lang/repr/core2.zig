const imp = @import("../../../imp.zig");
usingnamespace imp.common;
const meta = imp.meta;
const value = imp.lang.repr.value;

pub const Program = []const *const Expr;

pub const Expr = union(enum) {
    None,
    Some,
    Scalar: value.Scalar,
    Union: Pair,
    Intersect: Pair,
    Product: Pair,
    Equal: Pair,
    ScalarId: ScalarId,
    UnboxScalarId: ScalarId,
    SetId: SetId,
    Negate: *const Expr,
    Then: Then,
    Abstract: *const Expr,
    Apply: Pair,
    Box: Box,
    Fix: Fix,
    Reduce: Reduce,
    Enumerate: *const Expr,
    Annotate: Annotate,
    Native: Native,

    pub fn getChildren(self: Expr) FixedSizeArrayList(3, *const Expr) {
        var children = FixedSizeArrayList(3, *const Expr).init();
        inline for (@typeInfo(Expr).Union.fields) |expr_field, i| {
            if (@enumToInt(std.meta.activeTag(self)) == @typeInfo(@typeInfo(Expr).Union.tag_type.?).Enum.fields[i].value) {
                const T = expr_field.field_type;
                const v = @field(self, expr_field.name);
                switch (T) {
                    void, value.Scalar, ScalarId, Native => {},
                    *const Expr => children.append(v),
                    Pair, Then, Box, Fix, Reduce, Annotate => {
                        inline for (@typeInfo(T).Struct.fields) |value_field| {
                            if (value_field.field_type == *const Expr) {
                                children.append(@field(v, value_field.name));
                            }
                        }
                    },
                    else => @compileError("Missed case for " ++ @typeName(t)),
                }
            }
        }
        return children;
    }

    pub fn getChildrenMut(self: *Expr) FixedSizeArrayList(3, **const Expr) {
        var children = FixedSizeArrayList(3, **const Expr).init();
        inline for (@typeInfo(Expr).Union.fields) |expr_field, i| {
            if (@enumToInt(std.meta.activeTag(self.*)) == @typeInfo(@typeInfo(Expr).Union.tag_type.?).Enum.fields[i].value) {
                const T = expr_field.field_type;
                const v = &@field(self, expr_field.name);
                switch (T) {
                    void, value.Scalar, ScalarId, Native => {},
                    *const Expr => children.append(v),
                    Pair, Then, Box, Fix, Reduce, Annotate => {
                        inline for (@typeInfo(T).Struct.fields) |value_field| {
                            if (value_field.field_type == *const Expr) {
                                children.append(&@field(v, value_field.name));
                            }
                        }
                    },
                    else => @compileError("Missed case for " ++ @typeName(t)),
                }
            }
        }
        return children;
    }

    pub fn dumpInto(self: Expr, writer: anytype, indent: u32) anyerror!void {
        if (indent != 0) {
            try writer.writeAll("\n");
            try writer.writeByteNTimes(' ', indent);
        }
        switch (self) {
            .None => try writer.writeAll("none"),
            .Some => try writer.writeAll("some"),
            .Scalar => |scalar| try scalar.dumpInto(writer),
            .Union => try writer.writeAll("|"),
            .Intersect => try writer.writeAll("&"),
            .Product => try writer.writeAll(","),
            .Equal => try writer.writeAll("="),
            .ScalarId => |scalar_id| try std.fmt.format(writer, "s{}", .{scalar_id}),
            .UnboxScalarId => |scalar_id| try std.fmt.format(writer, "unbox s{}", .{scalar_id}),
            .SetId => |set_id| try std.fmt.format(writer, "S{}", .{set_id}),
            .Negate => try writer.writeAll("negate"),
            .Then => try writer.writeAll("then"),
            .Abstract => try writer.writeAll("?"),
            .Apply => try writer.writeAll("apply"),
            .Box => |box| {
                try std.fmt.format(writer, "box S{}", .{box.set_id});
                for (box.args) |scalar_id| {
                    try std.fmt.format(writer, " s{}", .{scalar_id});
                }
            },
            .Fix => try writer.writeAll("fix"),
            .Reduce => try writer.writeAll("reduce"),
            .Enumerate => try writer.writeAll("enumerate"),
            .Annotate => |annotate| try std.fmt.format(writer, "# {any}", .{annotate.annotation}),
            .Native => |native| try native.dumpInto(writer, indent),
        }
        for (self.getChildren().slice()) |child| {
            try child.dumpInto(writer, indent + 2);
        }
    }
};

// Index in program
pub const SetId = usize;

// De Bruijn index to some enclosing Abstract
pub const ScalarId = usize;

pub const Pair = struct {
    left: *const Expr,
    right: *const Expr,
};

pub const NameIx = usize;

pub const Then = struct {
    condition: *const Expr,
    true_branch: *const Expr,
};

pub const Box = struct {
    set_id: SetId,
    args: []ScalarId,
};

pub const Fix = struct {
    init: *const Expr,
    next: *const Expr,
};

pub const Reduce = struct {
    input: *const Expr,
    init: *const Expr,
    next: *const Expr,
};

pub const Annotate = struct {
    annotation: []const u8,
    body: *const Expr,
};

pub const Native = enum {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulus,
    Range,
    GreaterThan,
    GreaterThanOrEqual,

    pub fn toName(self: Native) []const u8 {
        return switch (self) {
            .Add => "+",
            .Subtract => "-",
            .Multiply => "*",
            .Divide => "/",
            .Modulus => "%",
            .Range => "range",
            .GreaterThan => ">",
            .GreaterThanOrEqual => ">=",
        };
    }

    pub fn fromName(name: []const u8) ?Native {
        inline for (@typeInfo(Native).Enum.fields) |field| {
            const native = @intToEnum(Native, field.value);
            if (meta.deepEqual(name, comptime native.toName())) return native;
        }
        return null;
    }

    pub fn dumpInto(self: Native, writer: anytype, _: u32) anyerror!void {
        try writer.writeAll(self.toName());
    }
};
