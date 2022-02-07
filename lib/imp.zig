const std = @import("std");
pub const util = @import("./imp/util.zig");

comptime {
    std.testing.refAllDecls(@This());
}

test {
    try std.testing.expect(true);
}
