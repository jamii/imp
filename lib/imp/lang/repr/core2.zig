const imp = @import("../../../imp.zig");
usingnamespace imp.common;
const meta = imp.meta;
const syntax = imp.lang.repr.syntax;
const value = imp.lang.repr.value2;

pub const Program = struct {
    def_exprs: []const *const Expr,

    pub fn dumpInto(self: Program, writer: anytype, indent: u32) anyerror!void {
        for (self.def_exprs) |def_expr, i| {
            if (i != 0) try writer.writeByteNTimes(' ', indent);
            try std.fmt.format(writer, "S{}:\n", .{i});
            try writer.writeByteNTimes(' ', indent + 4);
            try def_expr.dumpInto(writer, indent + 4);
            try writer.writeAll(";\n");
        }
    }
};

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
    DefId: DefId,
    Negate: *const Expr,
    Then: Then,
    Abstract: *const Expr,
    Apply: Pair,
    Box: Box,
    Fix: Fix,
    Reduce: Reduce,
    Enumerate: *const Expr,
    Annotate: Annotate,
    Watch: Watch,
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
                    Pair, Then, Box, Fix, Reduce, Annotate, Watch => {
                        inline for (@typeInfo(T).Struct.fields) |value_field| {
                            if (value_field.field_type == *const Expr) {
                                children.append(@field(v, value_field.name));
                            }
                        }
                    },
                    else => @compileError("Missed case for " ++ @typeName(T)),
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
                    Pair, Then, Box, Fix, Reduce, Annotate, Watch => {
                        inline for (@typeInfo(T).Struct.fields) |value_field| {
                            if (value_field.field_type == *const Expr) {
                                children.append(&@field(v, value_field.name));
                            }
                        }
                    },
                    else => @compileError("Missed case for " ++ @typeName(T)),
                }
            }
        }
        return children;
    }

    pub fn dumpInto(self: Expr, writer: anytype, indent: u32) anyerror!void {
        switch (self) {
            .None => try writer.writeAll("none"),
            .Some => try writer.writeAll("some"),
            .Scalar => |scalar| try scalar.dumpInto(writer, indent),
            .Union => try writer.writeAll("|"),
            .Intersect => try writer.writeAll("&"),
            .Product => try writer.writeAll(","),
            .Equal => try writer.writeAll("="),
            .ScalarId => |scalar_id| try std.fmt.format(writer, "s{}", .{scalar_id}),
            .UnboxScalarId => |scalar_id| try std.fmt.format(writer, "unbox s{}", .{scalar_id}),
            .DefId => |def_id| try std.fmt.format(writer, "S{}", .{def_id}),
            .Negate => try writer.writeAll("negate"),
            .Then => try writer.writeAll("then"),
            .Abstract => try writer.writeAll("?"),
            .Apply => try writer.writeAll("apply"),
            .Box => |box| {
                try std.fmt.format(writer, "box S{}", .{box.def_id});
                for (box.args) |scalar_id| {
                    try std.fmt.format(writer, " s{}", .{scalar_id});
                }
            },
            .Fix => |fix| {
                try std.fmt.format(writer, "fix S{}", .{fix.next.def_id});
                for (fix.next.args) |scalar_id| {
                    try std.fmt.format(writer, " s{}", .{scalar_id});
                }
            },
            .Reduce => |reduce| {
                try std.fmt.format(writer, "reduce S{}", .{reduce.next.def_id});
                for (reduce.next.args) |scalar_id| {
                    try std.fmt.format(writer, " s{}", .{scalar_id});
                }
            },
            .Enumerate => try writer.writeAll("enumerate"),
            .Annotate => |annotate| try std.fmt.format(writer, "# {any}", .{annotate.annotation}),
            .Watch => |watch| try watch.dumpInto(writer, indent),
            .Native => |native| try native.dumpInto(writer, indent),
        }
        for (self.getChildren().slice()) |child| {
            try writer.writeAll("\n");
            try writer.writeByteNTimes(' ', indent + 4);
            try child.dumpInto(writer, indent + 4);
        }
    }
};

// Index in program
pub const DefId = usize;

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
    def_id: DefId,
    args: []ScalarId,
};

pub const Fix = struct {
    init: *const Expr,
    next: Box,
};

pub const Reduce = struct {
    input: *const Expr,
    init: *const Expr,
    next: Box,
};

pub const Annotate = struct {
    annotation: []const u8,
    body: *const Expr,
};

pub const Watch = struct {
    expr: *const Expr,
    scope: []ScopeItem,

    pub const ScopeItem = struct {
        name: syntax.Name,
        scalar_id: ScalarId,
    };

    pub fn dumpInto(self: Watch, writer: anytype, _: u32) anyerror!void {
        try writer.writeAll("watch");
        for (self.scope) |scope_item|
            try std.fmt.format(writer, "#{s} s{}", .{ scope_item.name, scope_item.scalar_id });
    }
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
