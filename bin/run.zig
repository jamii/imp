const imp = @import("../lib/imp.zig");
usingnamespace imp.common;

pub fn main() anyerror ! void {
    var allocator = std.heap.c_allocator;
    var args = std.process.args();
    // arg 0 is executable
    _ = try args.next(allocator).?;
    while (args.next(allocator)) |arg_e| {
        const arg = try arg_e;

        var file = if (std.mem.eql(u8, arg, "-"))
            std.io.getStdIn()
        else
            try std.fs.cwd().openFile(arg, .{});

        // TODO can't use readFileAlloc on stdin
        var source = ArrayList(u8).init(allocator);
        defer source.deinit();
        {
            const chunk_size = 1024;
            var buf = try allocator.alloc(u8, chunk_size);
            defer allocator.free(buf);
            while (true) {
                const len = try file.readAll(buf);
                try source.appendSlice(buf[0..len]);
                if (len < chunk_size) break;
            }
        }

        var arena = ArenaAllocator.init(allocator);
        var error_info: ?imp.lang.InterpretErrorInfo = null;
        const result = imp.lang.interpret(&arena, source.items, &error_info);
        const out_stream = std.io.getStdOut().outStream();
        if (result) |type_and_set| {
            try out_stream.writeAll("type: ");
            try type_and_set.set_type.dumpInto(out_stream);
            try out_stream.writeAll("\nvalue: ");
            try type_and_set.set.dumpInto(allocator, out_stream);
            try out_stream.writeAll("\n");
        } else |err| {
            switch (err) {
                // TODO report source position

                error.ParseError => try std.fmt.format(out_stream, "Parse error: {}\n", .{error_info.?.Parse.message}),
                error.DesugarError => try std.fmt.format(out_stream, "Desugar error: {}\n", .{error_info.?.Desugar.message}),
                error.AnalyzeError => try std.fmt.format(out_stream, "Analyze error: {}\n", .{error_info.?.Analyze.message}),
                error.InterpretError => try std.fmt.format(out_stream, "Interpret error: {}\n", .{error_info.?.Interpret.message}),

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
    }
}
