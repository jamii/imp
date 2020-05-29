const imp = @import("../../../imp.zig");
usingnamespace imp.common;
const value = imp.lang.repr.value;

// ascii, non-empty
pub const Name = []const u8;

pub const Native = struct {
    name: Name,
    input_arity: usize,
    output_arity: usize,
    fun: fn([]const value.Scalar) NativeError ! value.Set,

    // equality on name only

    pub fn deepHashInto(hasher: var, self: Native) void {
        meta.deepHashInto(hasher, self.name);
    }

    pub fn deepCompare(self: Native, other: Native) meta.Ordering {
        return meta.deepCompare(self.name, other.name);
    }
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
    UnboxName: NameIx,
    When: When,
    Abstract: *const Expr,
    Apply: Pair,
    Box: Box,
    Annotate: Annotate,
    Native: Native,

    pub fn getChildren(self: Expr) FixedSizeArrayList(2, *const Expr) {
        var children = FixedSizeArrayList(2, *const Expr).init();
        inline for (@typeInfo(Expr).Union.fields) |expr_field| {
            if (@enumToInt(std.meta.activeTag(self)) == expr_field.enum_field.?.value) {
                const t = expr_field.field_type;
                const v = @field(self, expr_field.enum_field.?.name);
                if (t == void or t == value.Scalar or t == NameIx or t == Native) {
                    // nothing to do
                } else if (t == *const Expr) {
                    children.append(v);
                } else if (t == Pair or t == When or t == Box or t == Annotate) {
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

    pub fn getNameIxes(self: Expr, name_ixes: *DeepHashSet(NameIx)) error{OutOfMemory} ! void {
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

    pub fn dumpInto(self: Expr, out_stream: var, indent: u32) anyerror!void {
        if (indent != 0) {
            try out_stream.writeAll("\n");
            try out_stream.writeByteNTimes(' ', indent);
        }
        switch (self) {
            .None => try out_stream.writeAll("none"),
            .Some => try out_stream.writeAll("some"),
            .Scalar => |scalar| try scalar.dumpInto(out_stream),
            .Union => try out_stream.writeAll("|"),
            .Intersect => try out_stream.writeAll("&"),
            .Product => try out_stream.writeAll("."),
            .Equal => try out_stream.writeAll("="),
            .Name => |name_ix| try std.fmt.format(out_stream, "get {}", .{name_ix}),
            .UnboxName => |name_ix| try std.fmt.format(out_stream, "get unbox {}", .{name_ix}),
            .When => try out_stream.writeAll("when"),
            .Abstract => try out_stream.writeAll("\\ _ ->"),
            .Apply => try out_stream.writeAll("apply"),
            .Box => |box| {
                try out_stream.writeAll("box;");
                if (box.scope.len > 0) {
                    try std.fmt.format(out_stream, " {}", .{box.scope[0]});
                    for (box.scope[1..]) |name_ix, i| {
                        try std.fmt.format(out_stream, " . {}", .{name_ix});
                    }
                }
            },
            .Annotate => |annotate| try std.fmt.format(out_stream, "# {}", .{annotate.annotation}),
            .Native => |native| try out_stream.writeAll(native.name),
        }
        for (self.getChildren().slice()) |child| {
            try child.dumpInto(out_stream, indent+2);
        }
    }
};

pub const Pair = struct {
    left: *const Expr,
    right: *const Expr,
};

pub const NameIx = usize;

pub const When = struct {
    condition: *const Expr,
    true_branch: *const Expr,
};

pub const Box = struct {
    body: *const Expr,
    scope: []const NameIx,
};

pub const Annotate = struct {
    annotation: Name,
    body: *const Expr,
};
