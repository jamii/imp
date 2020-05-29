const imp = @import("../../../imp.zig");
usingnamespace imp.common;
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

    pub fn getChildren(self: Expr) FixedSizeArrayList(3, *const Expr) {
        var children = FixedSizeArrayList(3, *const Expr).init();
        inline for (@typeInfo(Expr).Union.fields) |expr_field| {
            if (@enumToInt(std.meta.activeTag(self)) == expr_field.enum_field.?.value) {
                const t = expr_field.field_type;
                const v = @field(self, expr_field.enum_field.?.name);
                if (t == void or t == value.Scalar or t == Name) {
                    // nothing to do
                } else if (t == *const Expr) {
                    children.append(v);
                } else if (t == Pair or t == When or t == Abstract or t == Annotate or t == If or t == Let or t == Lookup) {
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
            .Name => |name| try out_stream.writeAll(name),
            .When => try out_stream.writeAll("when"),
            .Abstract => |abstract| {
                try out_stream.writeAll("\\ ");
                for (abstract.args) |arg| {
                    try arg.dumpInto(out_stream);
                    try out_stream.writeAll(" ");
                }
                try out_stream.writeAll("->");
            },
            .Apply => try out_stream.writeAll("apply"),
            .Box => try out_stream.writeAll("[]"),
            .Annotate => |annotate| try std.fmt.format(out_stream, "# {}", .{annotate.annotation}),
            .Negate => try out_stream.writeAll("!"),
            .If => try out_stream.writeAll("when"),
            .Let => |let| try std.fmt.format(out_stream, "let {} =", .{let.name}),
            .Lookup => |lookup| try std.fmt.format(out_stream, ": {}", .{lookup.name}),
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

    fn dumpInto(self: Arg, out_stream: var) anyerror!void {
        if (self.unbox) {
            try std.fmt.format(out_stream, "[{}]", .{self.name});
        } else {
            try std.fmt.format(out_stream, "{}", .{self.name});
        }
    }
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
};
