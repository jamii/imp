const imp = @import("../lib/imp.zig");
usingnamespace imp.common;
const meta = imp.meta;

pub fn main() anyerror ! void {
    var allocator = std.heap.c_allocator;

    var num_tests: usize = 0;
    var num_failed: usize = 0;
    const cases = @embedFile("./end_to_end.test");

    var cases_iter = std.mem.split(cases, "\n\n");
    while (cases_iter.next()) |case| {
        var case_iter = std.mem.split(case, "---");
        const input = std.mem.trim(u8, case_iter.next().?, "\n ");
        const expected = std.mem.trim(u8, case_iter.next().?, "\n ");
        expect(case_iter.next() == null);

        var arena = ArenaAllocator.init(allocator);
        defer arena.deinit();

        var error_info: ?imp.lang.InterpretErrorInfo = null;
        const result = imp.lang.interpret(&arena, input, &error_info);

        var bytes = ArrayList(u8).init(allocator);
        defer bytes.deinit();
        const out_stream = bytes.outStream();
        if (result) |type_and_set| {
            try type_and_set.dumpInto(allocator, out_stream);
        } else |err| {
            try imp.lang.InterpretErrorInfo.dumpInto(error_info, err, out_stream);
        }
        const found = std.mem.trim(u8, bytes.items, "\n ");

        num_tests += 1;
        if (!std.mem.eql(u8, expected, found)) {
            num_failed += 1;
            warn("Source:\n{}\n\nExpected:\n{}\n\nFound:\n{}\n---\n\n", .{input, expected, found});
        }
    }

    if (num_failed > 0) {
        warn("{}/{} tests failed!\n", .{num_failed, num_tests});
        std.os.exit(1);
    } else {
        warn("All {} tests passed\n", .{num_tests});
        std.os.exit(0);
    }
}
