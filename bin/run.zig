const imp = @import("../lib/imp.zig");
usingnamespace imp.common;

pub fn main() anyerror!void {
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

        // TODO can't use readFileAlloc on stdin? try reader
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
        var desired_id: usize = 0;
        const interrupter = .{
            .current_id = 0,
            .desired_id = &desired_id,
        };
        var store = imp.lang.Store{
            .arena = &arena,
            .interrupter = interrupter,
            .source = source.items,
        };
        store.run();
        const writer = std.io.getStdOut().writer();
        try store.dumpInto(writer, 0);
    }
}
