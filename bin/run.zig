const std = @import("std");
const imp = @import("../lib/imp.zig");
const u = imp.util;

// Workflow looks like:
// `imp run imp.db`
//   runs all live rules in the db
// `imp checkout imp.db program.imp`
//   replaces contents of program.imp with all live rules in the db
// `imp diff imp.db program.imp`
//   prints the diff between imp.db and program.imp
// `imp checkin imp.db program.imp`
//   commits the diff between imp.db and program.imp to imp.db
//   updates program.imp to reflect any new rule_ids generated
// `imp pull imp0.db imp1.db`
//   merge changes from imp1.db into imp0.db

var arena = u.ArenaAllocator.init(std.heap.c_allocator);

pub fn main() !void {
    var args = std.process.args();
    // arg 0 is executable
    _ = try args.next(arena.allocator()).?;
    const command = try args.next(arena.allocator()).?;
    const db_path = try args.next(arena.allocator()).?;
    if (std.mem.eql(u8, command, "run")) {
        try run(db_path);
    } else if (std.mem.eql(u8, command, "checkout")) {
        const source_path = try args.next(arena.allocator()).?;
        try checkout(db_path, source_path);
    } else if (std.mem.eql(u8, command, "checkin")) {
        const source_path = try args.next(arena.allocator()).?;
        try checkin(db_path, source_path);
    } else {
        std.debug.print("Unknown command: {s}", .{command});
    }
}

pub fn run(db_path: [:0]const u8) !void {
    var storage = try imp.Storage.init(&arena, db_path);
    const source = try storage.getLiveSource();
    var runner = imp.Runner{ .arena = &arena };
    std.debug.print("{s}", .{runner.printed(runner.run(source))});
}

pub fn checkout(db_path: [:0]const u8, source_path: []const u8) !void {
    var storage = try imp.Storage.init(&arena, db_path);
    const source = try storage.getLiveSource();
    var source_file = if (std.mem.eql(u8, source_path, "-"))
        std.io.getStdOut()
    else
        try std.fs.cwd().createFile(source_path, .{ .truncate = true });
    try source_file.writeAll(source);
}

pub fn checkin(db_path: [:0]const u8, source_path: []const u8) !void {
    var storage = try imp.Storage.init(&arena, db_path);
    const old_inserts = try storage.getLiveInserts();

    var source_file = if (std.mem.eql(u8, source_path, "-"))
        std.io.getStdIn()
    else
        try std.fs.cwd().openFile(source_path, .{ .write = true });
    const old_source = try source_file.readToEndAlloc(arena.allocator(), std.math.maxInt(usize));
    var tokenizer = imp.Tokenizer{
        .arena = &arena,
        .source = old_source,
        .position = 0,
        .tokens = u.ArrayList(imp.Token).init(arena.allocator()),
        .error_info = null,
    };
    const tokens = try tokenizer.tokenize();
    var parser = imp.Parser{
        .arena = &arena,
        .tokens = tokens,
        .position = 0,
        .rules = u.ArrayList(imp.Rule).init(arena.allocator()),
        .error_info = null,
    };
    const program = try parser.parseProgram();

    // TODO can't actually diff because no rule_ids in source yet
    var rng = try newRng();
    const tx_id = rng.random().int(imp.Storage.TransactionId);
    var inserts = u.ArrayList(imp.Storage.Insert).init(arena.allocator());
    var deletes = u.ArrayList(imp.Storage.Delete).init(arena.allocator());
    for (old_inserts) |old_insert| {
        try deletes.append(.{
            .tx_id = tx_id,
            .rule_id = old_insert.rule_id,
        });
    }
    for (program.rules) |rule, rule_id| {
        var rule_source = u.ArrayList(u8).init(arena.allocator());
        try rule.printInto(rule_source.writer());
        try inserts.append(.{
            .tx_id = tx_id,
            .rule_id = @intCast(imp.Storage.RuleId, rule_id),
            .rule = rule_source.toOwnedSlice(),
        });
    }
    try storage.commit(inserts.items, deletes.items);

    const new_source = try storage.getLiveSource();
    try source_file.seekTo(0);
    try source_file.setEndPos(0);
    try source_file.writeAll(new_source);
}

fn newRng() !std.rand.DefaultCsprng {
    var seed: [32]u8 = undefined;
    try std.os.getrandom(&seed);
    var rng = std.rand.DefaultCsprng.init(seed);
    return rng;
}
