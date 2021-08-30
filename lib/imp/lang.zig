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

    pub fn error_range(self: ?InterpretErrorInfo, err: InterpretError) ?[2]usize {
        switch (err) {
            error.ParseError => {
                return [2]usize{ self.?.Parse.start, self.?.Parse.end };
            },
            error.DesugarError => {
                const syntax_meta = Store.getSyntaxMeta(self.?.Desugar.expr);
                return [2]usize{ syntax_meta.start, syntax_meta.end };
            },
            error.AnalyzeError => {
                const core_meta = Store.getCoreMeta(self.?.Analyze.expr);
                const syntax_meta = Store.getSyntaxMeta(core_meta.from);
                return [2]usize{ syntax_meta.start, syntax_meta.end };
            },
            error.InterpretError, error.NativeError => {
                const core_meta = Store.getCoreMeta(self.?.Interpret.expr);
                const syntax_meta = Store.getSyntaxMeta(core_meta.from);
                return [2]usize{ syntax_meta.start, syntax_meta.end };
            },
            else => return null,
        }
    }

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
    config: Config,

    // mutex protects these fields
    mutex: std.Thread.Mutex,
    // background loop waits for this
    state_changed_event: std.Thread.AutoResetEvent,
    // when set to true, the background thread will deinit its state and then exit
    should_deinit: bool,
    // new_request and new_response behave like queues, except that they only care about the most recent item
    new_request: ?Request,
    new_response: ?Response,
    // desired_id is the most recent id set in new_request
    // if the interpreter is running and notices that desired_id has changed, it gives up and returns error.WasInterrupted so we can start again with then new program
    // desired_id is only changed inside mutex but is read atomically by the Interrupter outside the mutex
    // TODO I think this is safe because it only ever increases and we don't care exactly how long it takes for the interpreter to notice
    desired_id: usize,

    pub const Config = struct {
        memory_limit_bytes: ?usize = null,
    };

    pub const Request = struct {
        id: usize,
        text: []const u8,
    };

    pub const Response = struct {
        id: usize,
        text: []const u8,
        error_range: ?[2]usize,
    };

    // --- called by outside thread ---

    pub fn init(allocator: *Allocator, config: Config) !*Worker {
        const self = try allocator.create(Worker);
        self.* = Worker{
            .allocator = allocator,
            .config = config,
            .mutex = .{},
            .state_changed_event = .{},
            .should_deinit = false,
            .new_request = null,
            .new_response = null,
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
        @atomicStore(usize, &self.desired_id, std.math.maxInt(usize), .SeqCst);
        self.state_changed_event.set();
    }

    pub fn setRequest(self: *Worker, request: Request) !void {
        const held = self.mutex.acquire();
        defer held.release();
        if (self.new_request) |old_request| self.allocator.free(old_request.text);
        self.new_request = request;
        self.new_request.?.text = try std.mem.dupe(self.allocator, u8, self.new_request.?.text);
        @atomicStore(usize, &self.desired_id, request.id, .SeqCst);
        self.state_changed_event.set();
    }

    pub fn getResponse(self: *Worker) ?Response {
        const held = self.mutex.acquire();
        defer held.release();
        if (self.new_response) |response| {
            self.new_response = null;
            return response;
        } else {
            return null;
        }
    }

    // --- called by worker thread ---

    fn deinit(self: *Worker) void {
        const held = self.mutex.acquire();
        defer held.release();
        if (self.new_request) |request| self.allocator.free(request.text);
        if (self.new_response) |response| self.allocator.free(response.text);
        self.allocator.destroy(self);
    }

    fn backgroundLoop(self: *Worker) !void {
        while (true) {
            // wait for a new request
            self.state_changed_event.wait();

            // get new request
            var new_request: Request = undefined;
            {
                const held = self.mutex.acquire();
                defer held.release();
                if (self.should_deinit) {
                    self.deinit();
                    return;
                }
                if (self.new_request) |request| {
                    new_request = request;
                    self.new_request = null;
                } else {
                    // spurious wakeup, wait again
                    // (can happen like this:
                    //  outside: sets request a, wakes worker
                    //  outside: sets request b, wakes worker
                    //  worker: wakes up, reads request b, sets request null
                    //  worker: wakes up, reads request null)
                    continue;
                }
            }
            defer self.allocator.free(new_request.text);

            // eval
            var gpa = std.heap.GeneralPurposeAllocator(.{
                // TODO it's possible but awkward to set this to false when config.memory_limit_bytes is null
                .enable_memory_limit = true,
            }){
                .backing_allocator = self.allocator,
                .requested_memory_limit = self.config.memory_limit_bytes orelse std.math.maxInt(usize),
            };
            defer _ = gpa.deinit();
            var arena = ArenaAllocator.init(&gpa.allocator);
            defer arena.deinit();
            var error_info: ?imp.lang.InterpretErrorInfo = null;
            const interrupter = Interrupter{
                .current_id = new_request.id,
                .desired_id = &self.desired_id,
            };
            const result = imp.lang.interpret(&arena, new_request.text, interrupter, &error_info);

            // print result
            var response_buffer = ArrayList(u8).init(self.allocator);
            defer response_buffer.deinit();
            var error_range: ?[2]usize = null;
            if (result) |type_and_set| {
                try type_and_set.dumpInto(&arena.allocator, response_buffer.writer());
            } else |err| {
                try InterpretErrorInfo.dumpInto(error_info, err, response_buffer.writer());
                error_range = InterpretErrorInfo.error_range(error_info, err);
            }

            // set response
            {
                const held = self.mutex.acquire();
                defer held.release();
                if (self.new_response) |old_response| self.allocator.free(old_response.text);
                self.new_response = .{
                    .text = response_buffer.toOwnedSlice(),
                    .id = new_request.id,
                    .error_range = error_range,
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
        // TODO probably don't need .SeqCst, but need to think hard about it
        if (@atomicLoad(usize, self.desired_id, .SeqCst) != self.current_id)
            return error.WasInterrupted;
    }
};
