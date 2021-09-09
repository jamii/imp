const imp = @import("../../../imp.zig");
usingnamespace imp.common;
const meta = imp.meta;
const value = imp.lang.repr.value;

// ascii, non-empty
pub const Name = []const u8;

pub const Expr = union(enum) {
    None,
    Some,
    Scalar: value.Scalar,
    Union: Pair,
    Intersect: Pair,
    Product: Pair,
    Equal: Pair,
    Name: NameIx,
    UnboxName: NameIx,
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
        inline for (@typeInfo(Expr).Union.fields) |expr_field| {
            if (@enumToInt(std.meta.activeTag(self)) == expr_field.enum_field.?.value) {
                const t = expr_field.field_type;
                const v = @field(self, expr_field.enum_field.?.name);
                if (t == void or t == value.Scalar or t == NameIx or t == Native) {
                    // nothing to do
                } else if (t == *const Expr) {
                    children.append(v);
                } else if (t == Pair or t == Then or t == Box or t == Fix or t == Reduce or t == Annotate) {
                    inline for (@typeInfo(t).Struct.fields) |value_field| {
                        if (value_field.field_type == *const Expr) {
                            children.append(@field(v, value_field.name));
                        }
                    }
                } else {
                    @compileError("Missed case for " ++ @typeName(t));
                }
            }
        }
        return children;
    }

    pub fn getChildrenMut(self: *Expr) FixedSizeArrayList(3, **const Expr) {
        var children = FixedSizeArrayList(3, **const Expr).init();
        inline for (@typeInfo(Expr).Union.fields) |expr_field| {
            if (@enumToInt(std.meta.activeTag(self.*)) == expr_field.enum_field.?.value) {
                const t = expr_field.field_type;
                var v = &@field(self, expr_field.enum_field.?.name);
                if (t == void or t == value.Scalar or t == NameIx or t == Native) {
                    // nothing to do
                } else if (t == *const Expr) {
                    children.append(v);
                } else if (t == Pair or t == Then or t == Box or t == Fix or t == Reduce or t == Annotate) {
                    inline for (@typeInfo(t).Struct.fields) |value_field| {
                        if (value_field.field_type == *const Expr) {
                            children.append(&@field(v, value_field.name));
                        }
                    }
                } else {
                    @compileError("Missed case for " ++ @typeName(t));
                }
            }
        }
        return children;
    }

    pub fn getNameIxes(self: Expr, name_ixes: *DeepHashSet(NameIx)) error{OutOfMemory}!void {
        switch (self) {
            .Name, .UnboxName => |name_ix| {
                _ = try name_ixes.put(name_ix, {});
            },
            else => {
                for (self.getChildren().slice()) |child| {
                    _ = try child.getNameIxes(name_ixes);
                }
            },
        }
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
            .Name => |name_ix| try std.fmt.format(writer, "get {}", .{name_ix}),
            .UnboxName => |name_ix| try std.fmt.format(writer, "get unbox {}", .{name_ix}),
            .Then => try writer.writeAll("then"),
            .Abstract => try writer.writeAll("?"),
            .Apply => try writer.writeAll("apply"),
            .Box => |box| {
                try writer.writeAll("@");
                if (box.scope.len > 0) {
                    try std.fmt.format(writer, " {}", .{box.scope[0]});
                    for (box.scope[1..]) |name_ix| {
                        try std.fmt.format(writer, " , {}", .{name_ix});
                    }
                }
            },
            .Annotate => |annotate| try std.fmt.format(writer, "# {}", .{annotate.annotation}),
            .Native => |native| try native.dumpInto(writer),
        }
        for (self.getChildren().slice()) |child| {
            try child.dumpInto(writer, indent + 2);
        }
    }
};

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
    body: *const Expr,
    scope: []const NameIx,
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
    annotation: Name,
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
