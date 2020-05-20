usingnamespace @import("./common.zig");

const syntax = @import("./syntax.zig");
const core = @import("./core.zig");

pub fn desugar(arena: *ArenaAllocator, syntax: *const syntax.Expr) OutOfMemory ! *const core.Expr {

}

// --------------------------------------------------------------------------------
