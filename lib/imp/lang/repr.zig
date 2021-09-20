const imp = @import("../../imp.zig");
usingnamespace imp.common;

pub const syntax = @import("./repr/syntax.zig");
pub const core = @import("./repr/core.zig");
pub const core2 = @import("./repr/core2.zig");
pub const type_ = @import("./repr/type.zig");
pub const value = @import("./repr/value.zig");

comptime {
    @import("std").testing.refAllDecls(@This());
}
