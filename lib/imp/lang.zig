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
            error.WasInterrupted => try std.fmt.format(out_stream, "Was interrupted\n", .{}),
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

pub fn interpret(arena: *ArenaAllocator, source: []const u8, interrupter: Interrupter, error_info: *?InterpretErrorInfo) InterpretError!TypeAndSet {
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
    const set_type = pass.analyze.analyze(&store, core_expr, interrupter, &analyze_error_info) catch |err| {
        if (err == error.AnalyzeError) {
            error_info.* = .{ .Analyze = analyze_error_info.? };
        }
        return err;
    };

    var interpret_error_info: ?pass.interpret.ErrorInfo = null;
    const set = pass.interpret.interpret(&store, arena, core_expr, interrupter, &interpret_error_info) catch |err| {
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
    // when set to true, the background thread will deinit its state and then exit
    should_deinit: bool,
    // new_program and new_result behave like queues, except that they only care about the most recent item
    new_program: ?TextAndId,
    new_result: ?TextAndId,
    // desired_id is the most recent id set in new_program
    // if the interpreter is running and notices that desired_id has changed, it gives up and returns error.WasInterrupted so we can start again with then new program
    // desired_id is only changed inside mutex but is read atomically by the Interrupter outside the mutex
    // TODO I think this is safe because it only ever increases and we don't care exactly how long it takes for the interpreter to notice
    desired_id: usize,

    pub const TextAndId = struct {
        text: []const u8,
        id: usize,
    };

    // --- called by outside thread ---

    pub fn init(allocator: *Allocator) !*Worker {
        const self = try allocator.create(Worker);
        self.* = Worker{
            .allocator = allocator,
            .mutex = .{},
            .state_changed_event = .{},
            .should_deinit = false,
            .new_program = null,
            .new_result = null,
            .desired_id = 0,
        };
        // spawn worker thread
        _ = try std.Thread.spawn(.{}, backgroundLoop, .{self});
        return self;
    }

    pub fn deinitSoon(self: *Worker) void {
        const held = self.mutex.acquire();
        defer held.release();
        self.should_deinit = true;
        self.desired_id = std.math.maxInt(usize);
        self.state_changed_event.set();
    }

    pub fn setProgram(self: *Worker, program: TextAndId) !void {
        const held = self.mutex.acquire();
        defer held.release();
        if (self.new_program) |old_program| self.allocator.free(old_program.text);
        self.new_program = program;
        self.new_program.?.text = try std.mem.dupe(self.allocator, u8, self.new_program.?.text);
        self.desired_id = program.id;
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

    // --- called by worker thread ---

    fn deinit(self: *Worker) void {
        const held = self.mutex.acquire();
        defer held.release();
        if (self.new_program) |program| self.allocator.free(program.text);
        if (self.new_result) |result| self.allocator.free(result.text);
        self.allocator.destroy(self);
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
                if (self.should_deinit) {
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
            const interrupter = Interrupter{
                .current_id = new_program.id,
                .desired_id = &self.desired_id,
            };
            const result = imp.lang.interpret(&arena, new_program.text, interrupter, &error_info);

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
                if (self.new_result) |old_result| self.allocator.free(old_result.text);
                self.new_result = .{
                    .text = result_buffer.toOwnedSlice(),
                    .id = new_program.id,
                };
            }
        }
    }
};

pub const Interrupter = struct {
    current_id: usize,
    desired_id: *usize,

    // called by interpreter while running
    pub fn check(self: Interrupter) error{WasInterrupted}!void {
        // TODO figure out what ordering is needed
        if (@atomicLoad(usize, self.desired_id, .Monotonic) != self.current_id)
            return error.WasInterrupted;
    }
};
