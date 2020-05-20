usingnamespace @import("./common.zig");
const value = @import("./value.zig");

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

    // pub fn getChildren(self: Expr,

    pub fn dumpInto(self: Expr, out_stream: var, indent: u32) DumpError!void {
        if (indent != 0) {
            try out_stream.writeAll("\n");
            try out_stream.writeByteNTimes(' ', indent);
        }
        switch (self) {
            .None => try out_stream.writeAll("none"),
            .Some => try out_stream.writeAll("some"),
            .Scalar => |scalar| try scalar.dumpInto(out_stream),
            .Union => |pair| {
                try out_stream.writeAll("|");
                try pair.left.dumpInto(out_stream, indent+2);
                try pair.right.dumpInto(out_stream, indent+2);
            },
            .Intersect => |pair| {
                try out_stream.writeAll("&");
                try pair.left.dumpInto(out_stream, indent+2);
                try pair.right.dumpInto(out_stream, indent+2);
            },
            .Product => |pair| {
                try out_stream.writeAll(".");
                try pair.left.dumpInto(out_stream, indent+2);
                try pair.right.dumpInto(out_stream, indent+2);
            },
            .Equal => |pair| {
                try out_stream.writeAll("=");
                try pair.left.dumpInto(out_stream, indent+2);
                try pair.right.dumpInto(out_stream, indent+2);
            },
            .Name => |name| try out_stream.writeAll(name),
            .When => |when| {
                try out_stream.writeAll("when");
                try when.condition.dumpInto(out_stream, indent+2);
                try when.true_branch.dumpInto(out_stream, indent+2);
            },
            .Abstract => |abstract| {
                try out_stream.writeAll("\\ ");
                for (abstract.args) |arg| {
                    try arg.dumpInto(out_stream);
                    try out_stream.writeAll(" ");
                }
                try out_stream.writeAll("->");
                try abstract.body.dumpInto(out_stream, indent+2);
            },
            .Apply => |pair| {
                try out_stream.writeAll("apply");
                try pair.left.dumpInto(out_stream, indent+2);
                try pair.right.dumpInto(out_stream, indent+2);
            },
            .Box => |expr| {
                try out_stream.writeAll("[]");
                try expr.dumpInto(out_stream, indent+2);
            },
            .Annotate => |annotate| {
                try std.fmt.format(out_stream, "# {}", .{annotate.annotation});
                try annotate.body.dumpInto(out_stream, indent+2);
            },
            .Negate => |expr| {
                try out_stream.writeAll("!");
                try expr.dumpInto(out_stream, indent+2);
            },
            .If => |if_| {
                try out_stream.writeAll("when");
                try if_.condition.dumpInto(out_stream, indent+2);
                try if_.true_branch.dumpInto(out_stream, indent+2);
                try if_.false_branch.dumpInto(out_stream, indent+2);
            },
            .Let => |let| {
                try std.fmt.format(out_stream, "let {} =", .{let.name});
                try let.value.dumpInto(out_stream, indent+2);
                try let.body.dumpInto(out_stream, indent+2);
            },
            .Lookup => |lookup| {
                try std.fmt.format(out_stream, ": {}", .{lookup.name});
                try lookup.value.dumpInto(out_stream, indent+2);
            },
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

    fn dumpInto(self: Arg, out_stream: var) DumpError!void {
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
