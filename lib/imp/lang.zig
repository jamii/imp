const imp = @import("../imp.zig");
usingnamespace imp.common;

pub const repr = @import("./lang/repr.zig");
pub const pass = @import("./lang/pass.zig");
pub const Store = @import("./lang/store.zig").Store;

comptime {
    @import("std").testing.refAllDecls(@This());
}

// TODO calculate source position
pub const InterpretErrorInfo = union(enum) {
    Parse: pass.parse.ErrorInfo,
    Desugar: pass.desugar.ErrorInfo,
    Analyze: pass.analyze.ErrorInfo,
    Interpret: pass.interpret.ErrorInfo,

    pub fn dumpInto(self: ?InterpretErrorInfo, err: InterpretError, out_stream: anytype) anyerror!void {
        switch (err) {
            // TODO report source position
            error.ParseError => try std.fmt.format(out_stream, "Parse error: {s}\n", .{self.?.Parse.message}),
            error.DesugarError => try std.fmt.format(out_stream, "Desugar error: {s}\n", .{self.?.Desugar.message}),
            error.AnalyzeError => try std.fmt.format(out_stream, "Analyze error: {s}\n", .{self.?.Analyze.message}),
            error.InterpretError => try std.fmt.format(out_stream, "Interpret error: {s}\n", .{self.?.Interpret.message}),
            error.NativeError => try std.fmt.format(out_stream, "Native error: {s}\n", .{self.?.Interpret.message}),

            error.Utf8InvalidStartByte, error.InvalidUtf8, error.InvalidCharacter, error.Utf8ExpectedContinuation, error.Utf8OverlongEncoding, error.Utf8EncodesSurrogateHalf, error.Utf8CodepointTooLarge => try std.fmt.format(out_stream, "Invalid utf8 input: {}\n", .{err}),

            error.OutOfMemory => try std.fmt.format(out_stream, "Out of memory\n", .{}),
        }
    }
};

pub const InterpretError = pass.parse.Error ||
    pass.desugar.Error ||
    pass.analyze.Error ||
    pass.interpret.Error;

pub const TypeAndSet = struct {
    set_type: repr.type_.SetType,
    set: repr.value.Set,

    pub fn dumpInto(self: TypeAndSet, allocator: *Allocator, out_stream: anytype) anyerror!void {
        try out_stream.writeAll("type:\n");
        try self.set_type.dumpInto(out_stream);
        try out_stream.writeAll("\nvalue:\n");
        try self.set.dumpInto(allocator, out_stream);
        try out_stream.writeAll("\n");
    }
};

pub fn interpret(arena: *ArenaAllocator, source: []const u8, error_info: *?InterpretErrorInfo) InterpretError!TypeAndSet {
    var store = Store.init(arena);

    var parse_error_info: ?pass.parse.ErrorInfo = null;
    const syntax_expr = pass.parse.parse(&store, source, &parse_error_info) catch |err| {
        if (err == error.ParseError) {
            error_info.* = .{ .Parse = parse_error_info.? };
        }
        return err;
    };

    var desugar_error_info: ?pass.desugar.ErrorInfo = null;
    const core_expr = pass.desugar.desugar(&store, syntax_expr, &desugar_error_info) catch |err| {
        if (err == error.DesugarError) {
            error_info.* = .{ .Desugar = desugar_error_info.? };
        }
        return err;
    };

    var analyze_error_info: ?pass.analyze.ErrorInfo = null;
    const set_type = pass.analyze.analyze(&store, core_expr, &analyze_error_info) catch |err| {
        if (err == error.AnalyzeError) {
            error_info.* = .{ .Analyze = analyze_error_info.? };
        }
        return err;
    };

    var interpret_error_info: ?pass.interpret.ErrorInfo = null;
    const set = pass.interpret.interpret(&store, arena, core_expr, &interpret_error_info) catch |err| {
        if (err == error.InterpretError or err == error.NativeError) {
            error_info.* = .{ .Interpret = interpret_error_info.? };
        }
        return err;
    };

    return TypeAndSet{
        .set_type = set_type,
        .set = set,
    };
}

pub const Worker = struct {
    allocator: *Allocator,

    // mutex protects these fields
    mutex: std.Thread.Mutex,
    // background loop waits for this
    state_changed_event: std.Thread.AutoResetEvent,
    // the background thread must check this before setting new_program or new_result
    background_loop_should_stop: bool,
    // new_program and new_result behave like queues, except that they only care about the most recent item
    new_program: ?TextAndId,
    new_result: ?TextAndId,

    pub const TextAndId = struct {
        text: []const u8,
        id: usize,
    };

    pub fn init(allocator: *Allocator) !*Worker {
        const self = try allocator.create(Worker);
        self.* = Worker{
            .allocator = allocator,
            .mutex = .{},
            .state_changed_event = .{},
            .background_loop_should_stop = false,
            .new_program = null,
            .new_result = null,
        };
        _ = try std.Thread.spawn(.{}, backgroundLoop, .{self});
        return self;
    }

    fn deinit(self: *Worker) void {
        const held = self.mutex.acquire();
        defer held.release();
        if (self.new_program) |program| self.allocator.free(program.text);
        if (self.new_result) |result| self.allocator.free(result.text);
        self.allocator.destroy(self);
    }

    pub fn stopSoon(self: *Worker) void {
        const held = self.mutex.acquire();
        defer held.release();
        self.background_loop_should_stop = true;
        self.state_changed_event.set();
    }

    pub fn setProgram(self: *Worker, program: TextAndId) !void {
        const held = self.mutex.acquire();
        defer held.release();
        if (self.new_program) |old_program| self.allocator.free(old_program.text);
        self.new_program = program;
        self.new_program.?.text = try std.mem.dupe(self.allocator, u8, self.new_program.?.text);
        self.state_changed_event.set();
    }

    pub fn getResult(self: *Worker) ?TextAndId {
        const held = self.mutex.acquire();
        defer held.release();
        if (self.new_result) |result| {
            self.new_result = null;
            return result;
        } else {
            return null;
        }
    }

    fn backgroundLoop(self: *Worker) !void {
        while (true) {
            // wait for a new program
            self.state_changed_event.wait();

            // get new program
            var new_program: TextAndId = undefined;
            {
                const held = self.mutex.acquire();
                defer held.release();
                if (self.background_loop_should_stop) {
                    self.deinit();
                    return;
                }
                new_program = self.new_program.?;
                self.new_program = null;
            }
            defer self.allocator.free(new_program.text);

            // eval
            var arena = ArenaAllocator.init(self.allocator);
            defer arena.deinit();
            var error_info: ?imp.lang.InterpretErrorInfo = null;
            const result = imp.lang.interpret(&arena, new_program.text, &error_info);

            // print result
            var result_buffer = ArrayList(u8).init(self.allocator);
            defer result_buffer.deinit();
            if (result) |type_and_set|
                try type_and_set.dumpInto(&arena.allocator, result_buffer.writer())
            else |err|
                try imp.lang.InterpretErrorInfo.dumpInto(error_info, err, result_buffer.writer());

            // set result
            {
                const held = self.mutex.acquire();
                defer held.release();
                if (self.background_loop_should_stop) {
                    self.deinit();
                    return;
                }
                if (self.new_result) |old_result| self.allocator.free(old_result.text);
                self.new_result = .{
                    .text = result_buffer.toOwnedSlice(),
                    .id = new_program.id,
                };
            }
        }
    }
};
