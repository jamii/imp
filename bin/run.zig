const std = @import("std");
const imp = @import("../lib/imp.zig");
const u = imp.util;

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
        const source = try file.reader().readAllAlloc(allocator, std.math.maxInt(usize));

        var arena = u.ArenaAllocator.init(allocator);
        var desired_id: usize = 0;
        const interrupter = .{
            .current_id = 0,
            .desired_id = &desired_id,
        };
        var store = imp.lang.Store{
            .arena = &arena,
            .interrupter = interrupter,
            .source = source,
        };
        store.run();
        const writer = std.io.getStdOut().writer();
        try store.dumpInto(writer, 0);
    }
}
