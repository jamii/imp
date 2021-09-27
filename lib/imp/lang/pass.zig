const imp = @import("../../imp.zig");
usingnamespace imp.common;

pub const parse = @import("./pass/parse.zig");
pub const desugar = @import("./pass/desugar.zig");
pub const desugar2 = @import("./pass/desugar2.zig");
pub const analyze = @import("./pass/analyze.zig");
pub const analyze2 = @import("./pass/analyze2.zig");
pub const interpret = @import("./pass/interpret.zig");
pub const interpret2 = @import("./pass/interpret2.zig");

comptime {
    @import("std").testing.refAllDecls(@This());
}
