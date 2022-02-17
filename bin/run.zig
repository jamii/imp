const std = @import("std");
const imp = @import("../lib/imp.zig");
const u = imp.util;

comptime {
    std.testing.refAllDecls(@This());
}

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
    if (std.mem.eql(u8, command, "gui")) {
        try doGui(db_path);
    } else if (std.mem.eql(u8, command, "run")) {
        try doRun(db_path);
    } else if (std.mem.eql(u8, command, "checkout")) {
        const source_path = try args.next(arena.allocator()).?;
        try doCheckout(db_path, source_path);
    } else if (std.mem.eql(u8, command, "diff")) {
        const source_path = try args.next(arena.allocator()).?;
        try doDiff(db_path, source_path);
    } else if (std.mem.eql(u8, command, "checkin")) {
        const source_path = try args.next(arena.allocator()).?;
        try doCheckin(db_path, source_path);
    } else if (std.mem.eql(u8, command, "pull")) {
        const remote_db_path = try args.next(arena.allocator()).?;
        try doPull(db_path, remote_db_path);
    } else {
        std.debug.print("Unknown command: {s}", .{command});
    }
}

pub fn doGui(db_path: [:0]const u8) !void {
    var storage = try imp.Storage.init(&arena, db_path);
    try imp.gui.run(&storage);
}

pub fn doRun(db_path: [:0]const u8) !void {
    var storage = try imp.Storage.init(&arena, db_path);
    const source = try storage.getLiveSource();
    var runner = imp.Runner{ .arena = &arena };
    const result = runner.run(source);
    if (runner.parser) |parser| {
        var rules_by_id = u.DeepHashMap(imp.RuleId, imp.Rule).init(arena.allocator());
        for (parser.rules.items) |rule| {
            if (try rules_by_id.fetchPut(rule.id.?, rule)) |kv| {
                var rule_source = u.ArrayList(u8).init(arena.allocator());
                try rule.printInto(rule_source.writer());
                var conflicting_rule_source = u.ArrayList(u8).init(arena.allocator());
                try kv.value.printInto(conflicting_rule_source.writer());
                std.debug.print("WARNING: Conflicting definitions for id {}.\n\nFirst rule:\n{s}\n\nSecond rule:\n{s}\n\n", .{ rule.id.?, rule_source.items, conflicting_rule_source.items });
            }
        }
    }
    std.debug.print("{s}", .{runner.printed(result)});
}

pub fn doCheckout(db_path: [:0]const u8, source_path: []const u8) !void {
    var storage = try imp.Storage.init(&arena, db_path);
    const source = try storage.getLiveSource();
    const source_file = try createSourceFile(source_path);
    try source_file.writeAll(source);
}

pub fn doDiff(db_path: [:0]const u8, source_path: []const u8) !void {
    var storage = try imp.Storage.init(&arena, db_path);
    const source_file = try openSourceFile(source_path);

    const diff = try getDiff(&storage, source_file);

    const stdout = std.io.getStdOut();
    const writer = stdout.writer();
    // TODO want to print deleted_tx_id too, but we compute that inside sqlite
    for (diff.deletes) |delete| {
        try delete.printInto(writer);
        try writer.writeAll("\n");
    }
    for (diff.inserts) |insert| {
        try insert.printInto(writer);
        try writer.writeAll("\n");
    }
}

pub fn doCheckin(db_path: [:0]const u8, source_path: []const u8) !void {
    var storage = try imp.Storage.init(&arena, db_path);
    const source_file = try openSourceFile(source_path);

    const diff = try getDiff(&storage, source_file);
    try storage.commit(diff.inserts, diff.deletes);

    const new_source = try storage.getLiveSource();
    try source_file.seekTo(0);
    try source_file.setEndPos(0);
    try source_file.writeAll(new_source);
}

pub fn doPull(db_path: [:0]const u8, remote_db_path: [:0]const u8) !void {
    var storage = try imp.Storage.init(&arena, db_path);
    var remote_storage = try imp.Storage.init(&arena, remote_db_path);

    try storage.pullFrom(&remote_storage);
}

const Diff = struct {
    inserts: []const imp.Storage.Insert,
    deletes: []const imp.Storage.Delete,
};

fn getDiff(storage: *imp.Storage, source_file: std.fs.File) !Diff {
    const old_inserts = try storage.getLiveInserts();

    // parse program
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

    // assign rule ids
    var rng = try newRng();
    for (parser.rules.items) |*new_rule| {
        if (new_rule.id == null) {
            new_rule.id = rng.random().int(imp.RuleId);
        }
    }

    // build indexes
    var old_inserts_by_rule_id = u.DeepHashMap(imp.RuleId, u.ArrayList(imp.Storage.Insert)).init(arena.allocator());
    for (old_inserts) |old_insert| {
        const entry = try old_inserts_by_rule_id.getOrPut(old_insert.rule_id);
        if (!entry.found_existing)
            entry.value_ptr.* = u.ArrayList(imp.Storage.Insert).init(arena.allocator());
        try entry.value_ptr.append(old_insert);
    }
    var new_rules_by_rule_id = u.DeepHashMap(imp.RuleId, u.ArrayList(imp.Rule)).init(arena.allocator());
    for (program.rules) |new_rule| {
        const entry = try new_rules_by_rule_id.getOrPut(new_rule.id.?);
        if (!entry.found_existing)
            entry.value_ptr.* = u.ArrayList(imp.Rule).init(arena.allocator());
        try entry.value_ptr.append(new_rule);
    }

    // diff
    const tx_id = rng.random().int(imp.Storage.TransactionId);
    var inserts = u.ArrayList(imp.Storage.Insert).init(arena.allocator());
    var deletes = u.ArrayList(imp.Storage.Delete).init(arena.allocator());
    for (old_inserts) |old_insert| {
        var still_exists = false;
        if (new_rules_by_rule_id.get(old_insert.rule_id)) |matching_new_rules| {
            for (matching_new_rules.items) |matching_new_rule| {
                var rule_source = u.ArrayList(u8).init(arena.allocator());
                try matching_new_rule.printInto(rule_source.writer());
                if (std.mem.eql(u8, old_insert.rule, rule_source.items)) {
                    still_exists = true;
                }
            }
        }
        if (!still_exists)
            try deletes.append(.{
                .tx_id = tx_id,
                .rule_id = old_insert.rule_id,
            });
    }
    for (program.rules) |new_rule| {
        var rule_source = u.ArrayList(u8).init(arena.allocator());
        try new_rule.printInto(rule_source.writer());
        var existed_before = false;
        if (old_inserts_by_rule_id.get(new_rule.id.?)) |matching_old_inserts| {
            for (matching_old_inserts.items) |matching_old_insert| {
                if (std.mem.eql(u8, matching_old_insert.rule, rule_source.items)) {
                    existed_before = true;
                }
            }
        }
        if (!existed_before)
            try inserts.append(.{
                .tx_id = tx_id,
                .rule_id = new_rule.id.?,
                .rule = rule_source.toOwnedSlice(),
            });
    }

    return Diff{
        .inserts = inserts.toOwnedSlice(),
        .deletes = deletes.toOwnedSlice(),
    };
}

fn openSourceFile(source_path: []const u8) !std.fs.File {
    return std.fs.cwd().openFile(source_path, .{ .write = true });
}

fn createSourceFile(source_path: []const u8) !std.fs.File {
    return std.fs.cwd().createFile(source_path, .{ .truncate = true });
}

pub fn newRng() !std.rand.DefaultCsprng {
    var seed: [32]u8 = undefined;
    try std.os.getrandom(&seed);
    var rng = std.rand.DefaultCsprng.init(seed);
    return rng;
}
