const parse = @import("parse.zig");

pub fn main() !void {
    try parse.test_compiles();
}
