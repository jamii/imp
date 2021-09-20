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
    Desugar2: pass.desugar2.ErrorInfo,
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
            error.Desugar2Error => {
                const syntax_meta = Store.getSyntaxMeta(self.?.Desugar2.expr);
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

    pub fn dumpInto(self: ?InterpretErrorInfo, err: InterpretError, writer: anytype) anyerror!void {
        switch (err) {
            // TODO report source position
            error.ParseError => try std.fmt.format(writer, "Parse error: {s}\n", .{self.?.Parse.message}),
            error.DesugarError => try std.fmt.format(writer, "Desugar error: {s}\n", .{self.?.Desugar.message}),
            error.Desugar2Error => try std.fmt.format(writer, "Desugar2 error: {s}\n", .{self.?.Desugar2.message}),
            error.AnalyzeError => try std.fmt.format(writer, "Analyze error: {s}\n", .{self.?.Analyze.message}),
            error.InterpretError => try std.fmt.format(writer, "Interpret error: {s}\n", .{self.?.Interpret.message}),
            error.NativeError => try std.fmt.format(writer, "Native error: {s}\n", .{self.?.Interpret.message}),

            error.Utf8InvalidStartByte, error.InvalidUtf8, error.InvalidCharacter, error.Utf8ExpectedContinuation, error.Utf8OverlongEncoding, error.Utf8EncodesSurrogateHalf, error.Utf8CodepointTooLarge => try std.fmt.format(writer, "Invalid utf8 input: {}\n", .{err}),

            error.OutOfMemory => try std.fmt.format(writer, "Out of memory\n", .{}),
            error.WasInterrupted => try std.fmt.format(writer, "Was interrupted\n", .{}),
        }
    }
};

pub const InterpretError = pass.parse.Error ||
    pass.desugar.Error ||
    pass.desugar2.Error ||
    pass.analyze.Error ||
    pass.interpret.Error;

pub const InterpretResult = struct {
    set_type: repr.type_.SetType,
    set: repr.value.Set,
    watch_results: DeepHashSet(pass.interpret.WatchResult),
    watch_range: ?[2]usize,

    pub fn dumpInto(self: InterpretResult, allocator: *Allocator, writer: anytype) anyerror!void {
        try writer.writeAll("type:\n");
        try self.set_type.dumpInto(writer);
        try writer.writeAll("\nvalue:\n");
        try self.set.dumpInto(allocator, writer);
        if (self.watch_range) |_| {
            try writer.writeAll("\nwatch:\n\n");
            var watch_results = ArrayList(pass.interpret.WatchResult).init(allocator);
            defer watch_results.deinit();
            var iter = self.watch_results.iterator();
            while (iter.next()) |entry| try watch_results.append(entry.key_ptr.*);
            std.sort.sort(pass.interpret.WatchResult, watch_results.items, {}, struct {
                fn lessThan(_: void, a: pass.interpret.WatchResult, b: pass.interpret.WatchResult) bool {
                    return imp.meta.deepCompare(a, b) == .LessThan;
                }
            }.lessThan);
            for (watch_results.items) |watch_result| {
                for (watch_result.time) |time, i| {
                    try std.fmt.format(writer, "fix{}: {}; ", .{ i, time });
                }
                if (watch_result.time.len > 0) try writer.writeAll("\n");
                var printed_scope = false;
                for (watch_result.scope) |arg_and_scalar| {
                    // TODO might want to print boxes when we have good scope detection and better box printing
                    if (arg_and_scalar.scalar != .Box) {
                        const maybe_box: []const u8 = if (arg_and_scalar.arg.unbox) "@" else "";
                        try std.fmt.format(writer, "{s}: {}{s}; ", .{
                            arg_and_scalar.arg.name,
                            arg_and_scalar.scalar,
                            maybe_box,
                        });
                        printed_scope = true;
                    }
                }
                if (printed_scope) try writer.writeAll("\n");
                try watch_result.set.dumpInto(allocator, writer);
                try writer.writeAll("\n");
            }
        }
    }
};

pub fn interpret(
    arena: *ArenaAllocator,
    source: []const u8,
    watch_selection: Store.SourceSelection,
    interrupter: Interrupter,
    error_info: *?InterpretErrorInfo,
) InterpretError!InterpretResult {
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

    var desugar2_error_info: ?pass.desugar2.ErrorInfo = null;
    const core2_expr = pass.desugar2.desugar(&store, syntax_expr, &desugar2_error_info) catch |err| {
        if (err == error.Desugar2Error) {
            error_info.* = .{ .Desugar2 = desugar2_error_info.? };
        }
        return err;
    };
    dump(core2_expr);

    var analyze_error_info: ?pass.analyze.ErrorInfo = null;
    const set_type = pass.analyze.analyze(&store, core_expr, interrupter, &analyze_error_info) catch |err| {
        if (err == error.AnalyzeError) {
            error_info.* = .{ .Analyze = analyze_error_info.? };
        }
        return err;
    };

    const watch_expr_o = store.findCoreExprAt(watch_selection);
    var watch_range: ?[2]usize = null;
    if (watch_expr_o) |watch_expr| {
        const watch_meta = Store.getSyntaxMeta(Store.getCoreMeta(watch_expr).from);
        watch_range = .{ watch_meta.start, watch_meta.end };
    }
    var watch_results = DeepHashSet(pass.interpret.WatchResult).init(&arena.allocator);
    var interpret_error_info: ?pass.interpret.ErrorInfo = null;
    const set = pass.interpret.interpret(&store, arena, core_expr, watch_expr_o, &watch_results, interrupter, &interpret_error_info) catch |err| {
        if (err == error.InterpretError or err == error.NativeError) {
            error_info.* = .{ .Interpret = interpret_error_info.? };
        }
        return err;
    };

    return InterpretResult{
        .set_type = set_type,
        .set = set,
        .watch_results = watch_results,
        .watch_range = watch_range,
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
        selection: Store.SourceSelection,
    };

    pub const Response = struct {
        id: usize,
        text: []const u8,
        kind: ResponseKind,
    };

    pub const ResponseKind = union(enum) {
        Ok: ?[2]usize,
        Err: ?[2]usize,
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
            const interrupter = Interrupter{
                .current_id = new_request.id,
                .desired_id = &self.desired_id,
            };
            var error_info: ?imp.lang.InterpretErrorInfo = null;
            const result = imp.lang.interpret(&arena, new_request.text, new_request.selection, interrupter, &error_info);

            // print result
            var response_buffer = ArrayList(u8).init(self.allocator);
            defer response_buffer.deinit();
            var response_kind: ResponseKind = undefined;
            if (result) |ok| {
                try ok.dumpInto(&arena.allocator, response_buffer.writer());
                response_kind = .{ .Ok = ok.watch_range };
            } else |err| {
                try InterpretErrorInfo.dumpInto(error_info, err, response_buffer.writer());
                response_kind = .{ .Err = InterpretErrorInfo.error_range(error_info, err) };
            }

            // set response
            {
                const held = self.mutex.acquire();
                defer held.release();
                if (self.new_response) |old_response| self.allocator.free(old_response.text);
                self.new_response = .{
                    .text = response_buffer.toOwnedSlice(),
                    .id = new_request.id,
                    .kind = response_kind,
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
