pub const common = @import("./imp/common.zig");
pub const lang = @import("./imp/lang.zig");

comptime {
    @import("std").testing.refAllDecls(@This());
}
