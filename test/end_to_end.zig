const std = @import("std");
const imp = @import("../lib/imp.zig");
const u = imp.util;

pub fn main() anyerror!void {
    var allocator = std.heap.c_allocator;

    var num_tests: usize = 0;
    var num_failed: usize = 0;

    var args = std.process.args();
    // arg 0 is executable
    _ = try args.next(allocator).?;
    while (args.next(allocator)) |arg| {
        const filename = try arg;

        // TODO This is a total hack.
        //      When using `--test-cmd` to run with rr, `zig run` also passes the location of the zig binary as an extra argument. I don't know how to turn this off.
        if (std.mem.endsWith(u8, filename, "zig")) continue;

        var file = if (std.mem.eql(u8, filename, "-"))
            std.io.getStdIn()
        else
            try std.fs.cwd().openFile(filename, .{});

        // TODO can't use readFileAlloc on stdin
        var cases = u.ArrayList(u8).init(allocator);
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

        var cases_iter = std.mem.split(u8, cases.items, "\n\n");
        while (cases_iter.next()) |case| {
            var case_iter = std.mem.split(u8, case, "---");
            const input = std.mem.trim(u8, case_iter.next().?, "\n ");
            const expected = std.mem.trim(u8, case_iter.next().?, "\n ");
            try u.expect(case_iter.next() == null);

            var arena = u.ArenaAllocator.init(allocator);
            defer arena.deinit();

            var desired_id: usize = 0;
            const interrupter = .{
                .current_id = 0,
                .desired_id = &desired_id,
            };
            var store = imp.lang.Store{
                .arena = &arena,
                .interrupter = interrupter,
                .source = input,
            };
            store.run();

            var bytes = u.ArrayList(u8).init(allocator);
            defer bytes.deinit();
            const writer = bytes.writer();
            try store.dumpInto(writer, 0);
            const found = std.mem.trim(u8, bytes.items, "\n ");

            num_tests += 1;
            if (!std.mem.eql(u8, expected, found)) {
                num_failed += 1;
                u.warn("Filename:\n\n{s}\nSource:\n{s}\n\nExpected:\n{s}\n\nFound:\n{s}\n\n---\n\n", .{ filename, input, expected, found });
            }
        }

        if (num_failed > 0) {
            u.warn("{}/{} tests failed!\n", .{ num_failed, num_tests });
            std.os.exit(1);
        } else {
            u.warn("All {} tests passed\n", .{num_tests});
            std.os.exit(0);
        }
    }
}
