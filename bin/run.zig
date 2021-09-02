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
        var error_info: ?imp.lang.InterpretErrorInfo = null;
        const result = imp.lang.interpret(&arena, source.items, 0, interrupter, &error_info);
        const writer = std.io.getStdOut().writer();
        if (result) |type_and_set| {
            try type_and_set.dumpInto(allocator, writer);
        } else |err| {
            try imp.lang.InterpretErrorInfo.dumpInto(error_info, err, writer);
        }
    }
}
