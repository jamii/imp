const imp = @import("../../../imp.zig");
const u = imp.util;
const value = imp.lang.repr.value;
const core = imp.lang.repr.core;

pub const Exprs = struct {
    exprs: []const *const Expr,

    pub fn dumpInto(self: Expr, writer: anytype, indent: usize) WriterError(@TypeOf(writer))!void {
        for (self.exprs) |expr, i| {
            try std.fmt.format("S{} = ", .{i});
            try expr.dumpInto(writer, indent);
            try writer.writeAll("\n");
        }
    }
};

pub const Expr = enum {
    Collect: *const SetExpr,
    Enumerate: *const SetExpr,
    Fix: [2]*const SetExpr,
    Reduce: [3]*const SetExpr,
    Equal: [2]*const SetExpr,

    pub fn dumpInto(self: Expr, writer: anytype, indent: usize) WriterError(@TypeOf(writer))!void {
        switch (self) {
            .Collect => |body| {
                try body.dumpInto(writer, indent);
            },
            .Enumerate => |body| {
                try writer.writeAll("enumerate ");
                try body.dumpInto(writer, indent);
            },
            .Fix => |fix| {
                try writer.writeAll("fix ");
                try fix[0].dumpInto(writer, indent);
                try writer.writeAll(" ");
                try fix[1].dumpInto(writer, indent);
            },
            .Reduce => |reduce| {
                try writer.writeAll("reduce ");
                try reduce[0].dumpInto(writer, indent);
                try writer.writeAll(" ");
                try reduce[1].dumpInto(writer, indent);
                try writer.writeAll(" ");
                try reduce[2].dumpInto(writer, indent);
            },
            .Equal => |equal| {
                try writer.writeAll("= ");
                try equal[0].dumpInto(writer, indent);
                try writer.writeAll(" ");
                try equal[1].dumpInto(writer, indent);
            },
        }
    }
};

pub const SetExpr = struct {
    args: []const NameIx,
    body: *const BoolExpr,

    pub fn dumpInto(self: SetExpr, writer: anytype, indent: usize) WriterError(@TypeOf(writer))!void {
        try writer.writeAll("(");
        for (self.args) |arg| {
            try std.fmt.format(writer, "?s{}", .{arg.inner});
            try writer.writeAll(" , ");
        }
        try body.dumpInto(writer, indent);
        try writer.writeAll(")");
    }
};

pub const BoolExpr = enum {
    None,
    Some,
    Union: [2]*const BoolExpr,
    Intersect: [2]*const BoolExpr,
    Negate: *const BoolExpr,
    Equal: [2]ScalarRef,
    Apply: Apply,

    pub fn dumpInto(self: BoolExpr, writer: anytype, indent: usize) WriterError(@TypeOf(writer))!void {
        switch (self) {
            .None => try writer.writeAll("none"),
            .Some => try writer.writeAll("some"),
            .Union => |pair| {
                try writer.writeAll("(");
                try pair[0].dumpInto(writer, indent);
                try writer.writeAll(" | ");
                try pair[1].dumpInto(writer, indent);
                try writer.writeAll(")");
            },
            .Intersect => |pair| {
                try writer.writeAll("(");
                try pair[0].dumpInto(writer, indent);
                try writer.writeAll(" & ");
                try pair[1].dumpInto(writer, indent);
                try writer.writeAll(")");
            },
            .Negate => |body| {
                try writer.writeAll("!");
                try body.dumpInto(writer, indent);
            },
            .Equal => |pair| {
                try writer.writeAll("(");
                try pair[0].dumpInto(writer, indent);
                try writer.writeAll(" = ");
                try pair[1].dumpInto(writer, indent);
                try writer.writeAll(")");
            },
            .Apply => |apply| {
                try apply.dumpInto(writer, indent);
            },
        }
    }
};

pub const Apply = struct {
    set: SetRef,
    args: []const ScalarRef,

    pub fn dumpInto(self: Apply, writer: anytype, indent: usize) WriterError(@TypeOf(writer))!void {
        try writer.writeAll("(");
        try self.set.dumpInto(writer, indent);
        for (self.args) |arg| {
            try arg.dumpInto(writer, indent);
            try writer.writeAll(" ");
        }
        try writer.writeAll(")");
    }
};

pub const SetRef = enum {
    Name: NameIx,
    Native: core.Native,

    pub fn dumpInto(self: SetRef, writer: anytype, indent: usize) WriterError(@TypeOf(writer))!void {
        switch (self) {
            .Name => |name_ix| {
                try std.fmt.format(writer, "S{}", .{name_ix});
            },
            .Native => |native| {
                try native.dumpInto(writer, indent);
            },
        }
    }
};

pub const ScalarRef = enum {
    Name: NameIx,
    Scalar: value.Scalar,

    pub fn dumpInto(self: ScalarRef, writer: anytype, indent: usize) WriterError(@TypeOf(writer))!void {
        switch (self) {
            .Name => |name_ix| {
                try std.fmt.format(writer, "s{}", .{name_ix});
            },
            .Scalar => |scalar| {
                try scalar.dumpInto(writer, indent);
            },
        }
    }
};

pub const NameIx = usize;
