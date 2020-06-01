const imp = @import("../imp.zig");
usingnamespace imp.common;

pub const repr = @import("./lang/repr.zig");
pub const pass = @import("./lang/pass.zig");
pub const Store = @import("./lang/store.zig").Store;

// TODO calculate source position
pub const InterpretErrorInfo = union(enum) {
    Parse: pass.parse.ErrorInfo,
    Desugar: pass.desugar.ErrorInfo,
    Analyze: pass.analyze.ErrorInfo,
    Interpret: pass.interpret.ErrorInfo,

    pub fn dumpInto(self: ?InterpretErrorInfo, err: InterpretError, out_stream: var) anyerror ! void {
        switch (err) {
            // TODO report source position

            error.ParseError => try std.fmt.format(out_stream, "Parse error: {}\n", .{self.?.Parse.message}),
            error.DesugarError => try std.fmt.format(out_stream, "Desugar error: {}\n", .{self.?.Desugar.message}),
            error.AnalyzeError => try std.fmt.format(out_stream, "Analyze error: {}\n", .{self.?.Analyze.message}),
            error.InterpretError => try std.fmt.format(out_stream, "Interpret error: {}\n", .{self.?.Interpret.message}),

            error.Utf8InvalidStartByte,
            error.InvalidUtf8,
            error.InvalidCharacter,
            error.Utf8ExpectedContinuation,
            error.Utf8OverlongEncoding,
            error.Utf8EncodesSurrogateHalf,
            error.Utf8CodepointTooLarge => try std.fmt.format(out_stream, "Invalid utf8 input: {}\n", .{err}),

            error.OutOfMemory => try std.fmt.format(out_stream, "Out of memory\n", .{}),
        }
    }
};

pub const InterpretError =
    pass.parse.Error ||
    pass.desugar.Error ||
    pass.analyze.Error ||
    pass.interpret.Error;

pub const TypeAndSet = struct {
    set_type: repr.type_.SetType,
    set: repr.value.Set,

    pub fn dumpInto(self: TypeAndSet, allocator: *Allocator, out_stream: var) anyerror ! void {
        try out_stream.writeAll("type: ");
        try self.set_type.dumpInto(out_stream);
        try out_stream.writeAll("\nvalue: ");
        try self.set.dumpInto(allocator, out_stream);
        try out_stream.writeAll("\n");
    }
};

pub fn interpret(arena: *ArenaAllocator, source: []u8, error_info: *?InterpretErrorInfo) InterpretError ! TypeAndSet {
    var store = Store.init(arena);

    var parse_error_info: ?pass.parse.ErrorInfo = null;
    const syntax_expr = pass.parse.parse(&store, source, &parse_error_info) catch |err| {
        if (err == error.ParseError) {
            error_info.* = .{.Parse = parse_error_info.?};
        }
        return err;
    };

    var desugar_error_info: ?pass.desugar.ErrorInfo = null;
    const core_expr = pass.desugar.desugar(&store, syntax_expr, &desugar_error_info) catch |err| {
        if (err == error.DesugarError) {
            error_info.* = .{.Desugar = desugar_error_info.?};
        }
        return err;
    };

    var analyze_error_info: ?pass.analyze.ErrorInfo = null;
    const set_type = pass.analyze.analyze(&store, core_expr, &analyze_error_info) catch |err| {
        if (err == error.AnalyzeError) {
            error_info.* = .{.Analyze = analyze_error_info.?};
        }
        return err;
    };

    var interpret_error_info: ?pass.interpret.ErrorInfo = null;
    const set = pass.interpret.interpret(&store, arena, core_expr, &interpret_error_info) catch |err| {
        if (err == error.InterpretError) {
            error_info.* = .{.Interpret = interpret_error_info.?};
        }
        return err;
    };

    return TypeAndSet{
        .set_type = set_type,
        .set = set,
    };
}
