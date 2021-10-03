const imp = @import("../../../imp.zig");
usingnamespace imp.common;
const meta = imp.meta;
const syntax = imp.lang.repr.syntax;
const value = imp.lang.repr.value;

pub const Program = struct {
    exprs: []const Expr,
    from_syntax: []const syntax.ExprId,
    defs: []const ExprId,

    pub fn dumpInto(self: Program, writer: anytype, indent: u32) anyerror!void {
        for (self.defs) |expr_id, def_id| {
            if (def_id != 0) try writer.writeByteNTimes(' ', indent);
            try std.fmt.format(writer, "{}:\n", DefId{ .id = def_id });
            try writer.writeByteNTimes(' ', indent + 4);
            try self.dumpExprInto(expr_id, writer, indent + 4);
            try writer.writeAll(";\n");
        }
    }

    pub fn dumpExprInto(self: Program, expr_id: ExprId, writer: anytype, indent: u32) anyerror!void {
        const expr = self.exprs[expr_id.id];
        try expr.dumpInto(writer, indent);
        for (expr.getChildren().slice()) |child| {
            try writer.writeAll("\n");
            try writer.writeByteNTimes(' ', indent + 4);
            try self.dumpExprInto(child, writer, indent + 4);
        }
    }
};

// Index into Program.exprs/from_syntax
pub const ExprId = Id("e");

// Index into Program.defs
pub const DefId = Id("d");

// De Bruijn index to some enclosing Abstract
pub const ScalarId = Id("s");

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
    Negate: ExprId,
    Then: Then,
    Abstract: ExprId,
    Apply: Pair,
    Box: Box,
    Fix: Fix,
    Reduce: Reduce,
    Enumerate: ExprId,
    Annotate: Annotate,
    Watch: Watch,
    Native: Native,

    pub fn getChildren(self: Expr) FixedSizeArrayList(2, ExprId) {
        var children = FixedSizeArrayList(2, ExprId){};
        inline for (@typeInfo(Expr).Union.fields) |expr_field, i| {
            if (@enumToInt(std.meta.activeTag(self)) == @typeInfo(@typeInfo(Expr).Union.tag_type.?).Enum.fields[i].value) {
                const T = expr_field.field_type;
                const v = @field(self, expr_field.name);
                switch (T) {
                    void, value.Scalar, ScalarId, DefId, Native => {},
                    ExprId => children.append(v),
                    Pair, Then, Box, Fix, Reduce, Annotate, Watch => {
                        inline for (@typeInfo(T).Struct.fields) |value_field| {
                            if (value_field.field_type == ExprId) {
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

    pub fn dumpInto(self: Expr, writer: anytype, indent: u32) anyerror!void {
        switch (self) {
            .None => try writer.writeAll("none"),
            .Some => try writer.writeAll("some"),
            .Scalar => |scalar| try scalar.dumpInto(writer, indent),
            .Union => try writer.writeAll("|"),
            .Intersect => try writer.writeAll("&"),
            .Product => try writer.writeAll(","),
            .Equal => try writer.writeAll("="),
            .ScalarId => |scalar_id| try std.fmt.format(writer, "{}", .{scalar_id}),
            .UnboxScalarId => |scalar_id| try std.fmt.format(writer, "unbox {}", .{scalar_id}),
            .DefId => |def_id| try std.fmt.format(writer, "{}", .{def_id}),
            .Negate => try writer.writeAll("negate"),
            .Then => try writer.writeAll("then"),
            .Abstract => try writer.writeAll("?"),
            .Apply => try writer.writeAll("apply"),
            .Box => |box| {
                try std.fmt.format(writer, "box {}", .{box.def_id});
                for (box.args) |scalar_id| {
                    try std.fmt.format(writer, " {}", .{scalar_id});
                }
            },
            .Fix => |fix| {
                try std.fmt.format(writer, "fix {}", .{fix.next.def_id});
                for (fix.next.args) |scalar_id| {
                    try std.fmt.format(writer, " {}", .{scalar_id});
                }
            },
            .Reduce => |reduce| {
                try std.fmt.format(writer, "reduce {}", .{reduce.next.def_id});
                for (reduce.next.args) |scalar_id| {
                    try std.fmt.format(writer, " {}", .{scalar_id});
                }
            },
            .Enumerate => try writer.writeAll("enumerate"),
            .Annotate => |annotate| try std.fmt.format(writer, "# {any}", .{annotate.annotation}),
            .Watch => |watch| try watch.dumpInto(writer, indent),
            .Native => |native| try native.dumpInto(writer, indent),
        }
    }
};

pub const Pair = struct {
    left: ExprId,
    right: ExprId,
};

pub const NameIx = usize;

pub const Then = struct {
    condition: ExprId,
    true_branch: ExprId,
};

pub const Box = struct {
    def_id: DefId,
    args: []ScalarId,
};

pub const Fix = struct {
    init: ExprId,
    next: Box,
};

pub const Reduce = struct {
    input: ExprId,
    init: ExprId,
    next: Box,
};

pub const Annotate = struct {
    annotation: []const u8,
    body: ExprId,
};

pub const Watch = struct {
    scope: []ScopeItem,
    body: ExprId,

    pub const ScopeItem = struct {
        name: syntax.Name,
        scalar_id: ScalarId,
    };

    pub fn dumpInto(self: Watch, writer: anytype, _: u32) anyerror!void {
        try writer.writeAll("watch");
        for (self.scope) |scope_item|
            try std.fmt.format(writer, "#{s} {}", .{ scope_item.name, scope_item.scalar_id });
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
