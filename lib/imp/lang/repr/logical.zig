const imp = @import("../../../imp.zig");
usingnamespace imp.common;
const meta = imp.meta;
const value = imp.lang.repr.value;
const core = imp.lang.repr.core;

pub const Exprs = struct {
    exprs: []const Expr,

    pub fn dumpInto(self: Expr, out_stream: anytype) OutStreamError(@TypeOf(out_stream))!void {
        for (self.exprs) |expr, i| {
            try std.fmt.format("S{} = ", i);
            try expr.dumpInto(out_stream);
            try out_stream.writeAll("\n");
        }
    }
};

pub const Expr = enum {
    Collect: *const SetExpr,
    Enumerate: *const SetExpr,
    Fix: [2]*const SetExpr,
    Reduce: [3]*const SetExpr,

    pub fn dumpInto(self: Expr, out_stream: anytype) OutStreamError(@TypeOf(out_stream))!void {
        switch (self) {
            .Collect => |body| {
                try body.dumpInto(out_stream);
            },
            .Enumerate => |body| {
                try out_stream.writeAll("enumerate ");
                try body.dumpInto(out_stream);
            },
            .Fix => |fix| {
                try out_stream.writeAll("fix ");
                try fix[0].dumpInto(out_stream);
                try out_stream.writeAll(" ");
                try fix[1].dumpInto(out_stream);
            },
            .Reduce => |reduce| {
                try out_stream.writeAll("reduce ");
                try fix[0].dumpInto(out_stream);
                try out_stream.writeAll(" ");
                try fix[1].dumpInto(out_stream);
                try out_stream.writeAll(" ");
                try fix[2].dumpInto(out_stream);
            },
        }
    }
};

pub const SetExpr = struct {
    args: []const NameIx,
    body: BoolExpr,

    pub fn dumpInto(self: SetExpr, out_stream: anytype) OutStreamError(@TypeOf(out_stream))!void {
        try out_stream.writeAll("(");
        for (self.args) |arg| {
            try std.fmt.format(out_stream, "?s{}", .{arg.inner});
            try out_stream.writeAll(" . ");
        }
        try body.dumpInto(out_stream);
        try out_stream.writeAll(")");
    }
};

pub const BoolExpr = enum {
    None,
    Some,
    Union: [2]*const BoolExpr,
    Intersect: [2]*const BoolExpr,
    Negate: *const BoolExpr,
    ScalarEqual: [2]ScalarRef,
    Apply: Apply,

    pub fn dumpInto(self: BoolExpr, out_stream: anytype) OutStreamError(@TypeOf(out_stream))!void {
        switch (self) {
            .None => try out_stream.writeAll("none"),
            .Some => try out_stream.writeAll("some"),
            .Union => |pair| {
                try out_stream.writeAll("(");
                try pair[0].dumpInto(out_stream);
                try out_stream.writeAll(" | ");
                try pair[1].dumpInto(out_stream);
                try out_stream.writeAll(")");
            },
            .Intersect => |pair| {
                try out_stream.writeAll("(");
                try pair[0].dumpInto(out_stream);
                try out_stream.writeAll(" & ");
                try pair[1].dumpInto(out_stream);
                try out_stream.writeAll(")");
            },
            .Negate => |body| {
                try out_stream.writeAll("!");
                try body.dumpInto(out_stream);
            },
            .ScalarEqual => |pair| {
                try out_stream.writeAll("(");
                try pair[0].dumpInto(out_stream);
                try out_stream.writeAll(" ");
                try pair[1].dumpInto(out_stream);
                try out_stream.writeAll(")");
            },
            .Apply => |apply| {
                try apply.dumpInto(out_stream);
            },
        }
    }
};

pub const Apply = struct {
    set: SetRef,
    args: []const ScalarRef,

    pub fn dumpInto(self: Apply, out_stream: anytype) OutStreamError(@TypeOf(out_stream))!void {
        try out_stream.writeAll("(");
        try self.set.dumpInto(out_stream);
        for (self.args) |arg| {
            try arg.dumpInto(out_stream);
            try out_stream.writeAll(" ");
        }
        try out_stream.writeAll(")");
    }
};

pub const SetRef = enum {
    Name: NameIx,
    Native: core.Native,

    pub fn dumpInto(self: SetRef, out_stream: anytype) OutStreamError(@TypeOf(out_stream))!void {
        switch (self) {
            .Name => |name_ix| {
                try std.fmt.format(out_stream, "s{}", name_ix.inner);
            },
            .Native => |native| {
                try native.dumpInto(out_stream);
            },
        }
    }
};

pub const ScalarRef = enum {
    Name: NameIx,
    Scalar: value.Scalar,

    pub fn dumpInto(self: ScalarRef, out_stream: anytype) OutStreamError(@TypeOf(out_stream))!void {
        switch (self) {
            .Name => |name_ix| {
                try std.fmt.format(out_stream, "S{}", name_ix.inner);
            },
            .Scalar => |scalar| {
                try scalar.dumpInto(out_stream);
            },
        }
    }
};

pub const NameIx = struct {
    inner: usize,
};
