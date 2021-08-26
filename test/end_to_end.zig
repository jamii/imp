const imp = @import("../lib/imp.zig");
usingnamespace imp.common;
const meta = imp.meta;

pub fn main() anyerror!void {
    var allocator = std.heap.c_allocator;

    var num_tests: usize = 0;
    var num_failed: usize = 0;

    var args = std.process.args();
    // arg 0 is executable
    _ = try args.next(allocator).?;
    while (args.next(allocator)) |arg| {
        const filename = try arg;

        var file = if (std.mem.eql(u8, filename, "-"))
            std.io.getStdIn()
        else
            try std.fs.cwd().openFile(filename, .{});

        // TODO can't use readFileAlloc on stdin
        var cases = ArrayList(u8).init(allocator);
        defer cases.deinit();
        {
            const chunk_size = 1024;
            var buf = try allocator.alloc(u8, chunk_size);
            defer allocator.free(buf);
            while (true) {
                const len = try file.readAll(buf);
                try cases.appendSlice(buf[0..len]);
                if (len < chunk_size) break;
            }
        }

        var cases_iter = std.mem.split(cases.items, "\n\n");
        while (cases_iter.next()) |case| {
            var case_iter = std.mem.split(case, "---");
            const input = std.mem.trim(u8, case_iter.next().?, "\n ");
            const expected = std.mem.trim(u8, case_iter.next().?, "\n ");
            try expect(case_iter.next() == null);

            var arena = ArenaAllocator.init(allocator);
            defer arena.deinit();

            var desired_id: usize = 0;
            const interrupter = .{
                .current_id = 0,
                .desired_id = &desired_id,
            };
            var error_info: ?imp.lang.InterpretErrorInfo = null;
            const result = imp.lang.interpret(&arena, input, interrupter, &error_info);

            var bytes = ArrayList(u8).init(allocator);
            defer bytes.deinit();
            const writer = bytes.writer();
            if (result) |type_and_set| {
                try type_and_set.dumpInto(allocator, writer);
            } else |err| {
                try imp.lang.InterpretErrorInfo.dumpInto(error_info, err, writer);
            }
            const found = std.mem.trim(u8, bytes.items, "\n ");

            num_tests += 1;
            if (!std.mem.eql(u8, expected, found)) {
                num_failed += 1;
                warn("Filename:\n\n{s}\nSource:\n{s}\n\nExpected:\n{s}\n\nFound:\n{s}\n\n---\n\n", .{ filename, input, expected, found });
            }
        }

        if (num_failed > 0) {
            warn("{}/{} tests failed!\n", .{ num_failed, num_tests });
            std.os.exit(1);
        } else {
            warn("All {} tests passed\n", .{num_tests});
            std.os.exit(0);
        }
    }
}
