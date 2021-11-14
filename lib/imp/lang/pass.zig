const imp = @import("../../imp.zig");
const u = imp.util;

pub const parse = @import("./pass/parse.zig");
pub const desugar = @import("./pass/desugar.zig");
pub const analyze = @import("./pass/analyze.zig");
pub const interpret = @import("./pass/interpret.zig");

comptime {
    @import("std").testing.refAllDecls(@This());
}
