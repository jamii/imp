const imp = @import("../../../imp.zig");
usingnamespace imp.common;
const value = imp.lang.repr.value;

pub const Program = struct {
    exprs: []const Expr,
    from_source: []const [2]usize,

    pub fn dumpInto(self: Program, writer: anytype, indent: u32) anyerror!void {
        try self.dumpExprInto(.{ .id = self.exprs.len - 1 }, writer, indent);
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

// Index into Progam.exprs/from_source
pub const ExprId = Id("e");

// ascii, non-empty
pub const Name = []const u8;

pub const Expr = union(enum) {
    None,
    Some,
    Scalar: value.Scalar,
    Union: Pair,
    Intersect: Pair,
    Product: Pair,
    Extend: Pair,
    Equal: Pair,
    Name: Name,
    Def: Def,
    Negate: ExprId,
    Then: Then,
    ThenElse: ThenElse,
    Abstract: Abstract,
    Apply: Pair,
    Box: ExprId,
    Fix: Fix,
    Reduce: Reduce,
    Enumerate: ExprId,
    Annotate: Annotate,

    pub fn getChildren(self: Expr) FixedSizeArrayList(3, ExprId) {
        var children = FixedSizeArrayList(3, ExprId){};
        inline for (@typeInfo(Expr).Union.fields) |expr_field, i| {
            if (@enumToInt(std.meta.activeTag(self)) == @typeInfo(@typeInfo(Expr).Union.tag_type.?).Enum.fields[i].value) {
                const T = expr_field.field_type;
                const v = @field(self, expr_field.name);
                if (T == void or T == value.Scalar or T == Name) {
                    // nothing to do
                } else if (T == ExprId) {
                    children.append(v);
                } else if (T == Pair or T == Fix or T == Then or T == Fix or T == Reduce or T == Abstract or T == Annotate or T == ThenElse or T == Def) {
                    inline for (@typeInfo(T).Struct.fields) |value_field| {
                        if (value_field.field_type == ExprId) {
                            children.append(@field(v, value_field.name));
                        }
                    }
                } else {
                    @compileError("Missed case for " ++ @typeName(T));
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
            .Extend => try writer.writeAll("."),
            .Equal => try writer.writeAll("="),
            .Name => |name| try writer.writeAll(name),
            .Def => |def| try std.fmt.format(writer, "{s}:", .{def.name}),
            .Negate => try writer.writeAll("!"),
            .Then => try writer.writeAll("then"),
            .ThenElse => try writer.writeAll("then_else"),
            .Abstract => |abstract| try std.fmt.format(
                writer,
                "?{s}{s}",
                .{
                    if (abstract.arg.unbox) @as([]const u8, "@") else "",
                    abstract.arg.name,
                },
            ),
            .Apply => try writer.writeAll("apply"),
            .Box => try writer.writeAll("@"),
            .Fix => try writer.writeAll("fix"),
            .Reduce => try writer.writeAll("reduce"),
            .Enumerate => try writer.writeAll("enumerate"),
            .Annotate => |annotate| try std.fmt.format(writer, "# {s}", .{annotate.annotation}),
        }
    }
};

pub const Pair = struct {
    left: ExprId,
    right: ExprId,
};

pub const Then = struct {
    condition: ExprId,
    true_branch: ExprId,
};

pub const Abstract = struct {
    arg: Arg,
    body: ExprId,
};

pub const Arg = struct {
    name: Name,
    unbox: bool,
};

pub const Fix = struct {
    init: ExprId,
    next: ExprId,
};

pub const Reduce = struct {
    input: ExprId,
    init: ExprId,
    next: ExprId,
};

pub const Annotate = struct {
    annotation: Name,
    body: ExprId,
};

pub const ThenElse = struct {
    condition: ExprId,
    true_branch: ExprId,
    false_branch: ExprId,
};

pub const Def = struct {
    fix: bool,
    name: Name,
    value: ExprId,
    body: ExprId,
};
