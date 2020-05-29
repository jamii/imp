const imp = @import("../../imp.zig");
usingnamespace imp.common;

pub const parse = @import("./pass/parse.zig");
pub const desugar = @import("./pass/desugar.zig");
pub const analyze = @import("./pass/analyze.zig");
pub const interpet = @import("./pass/interpret.zig");
