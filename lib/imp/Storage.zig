const std = @import("std");
const imp = @import("../imp.zig");
const u = imp.util;

const c = @cImport({
    @cInclude("sqlite3.h");
});

const Self = @This();

arena: *u.ArenaAllocator,
db_path: [:0]const u8,
db: *c.sqlite3,

// Temporary - has to be u52 to fit into imp.Atom.Number
pub const TransactionId = u52;
pub const RuleId = u52;

pub const Insert = struct {
    tx_id: TransactionId,
    rule_id: RuleId,
    rule: []const u8,

    pub fn printInto(self: Insert, writer: anytype) !void {
        try std.fmt.format(writer, "insert({}, {}, \"{}\").", .{ self.tx_id, self.rule_id, std.zig.fmtEscapes(self.rule) });
    }
};

pub const Delete = struct {
    tx_id: TransactionId,
    rule_id: RuleId,

    pub fn printInto(self: Delete, writer: anytype) !void {
        try std.fmt.format(writer, "delete({}, {}).", .{ self.tx_id, self.rule_id });
    }
};

pub fn init(arena: *u.ArenaAllocator, db_path: [:0]const u8) !Self {
    // Open db.
    var db: ?*c.sqlite3 = null;
    const result = c.sqlite3_open_v2(
        db_path,
        &db,
        c.SQLITE_OPEN_READWRITE | c.SQLITE_OPEN_CREATE,
        null,
    );
    if (db == null) {
        // https://www.sqlite.org/c3ref/open.html
        // > if SQLite is unable to allocate memory to hold the sqlite3 object, a NULL will be written into *ppDb
        u.panic("Sqlite OOM", .{});
    }
    check_sqlite_error(db.?, result);

    var self = Self{
        .arena = arena,
        .db_path = db_path,
        .db = db.?,
    };

    // Ensure that transactions table exists.
    _ = try self.query(
        \\ create table if not exists imp_inserts(tx_id integer, rule_id integer, rule text);
    , &.{});
    _ = try self.query(
        \\ create table if not exists imp_deletes(tx_id integer, rule_id integer, deleted_tx_id integer);
    , &.{});

    return self;
}

pub fn deinit(self: Self) void {
    _ = c.sqlite3_close(self.db);
}

pub fn commit(self: *Self, inserts: []const Insert, deletes: []const Delete) !void {
    _ = try self.query(
        \\ begin transaction;
    , &.{});
    errdefer _ = self.query(
        \\ rollback transaction;
    , &.{}) catch {};
    for (deletes) |delete|
        // insert a row into imp_deletes for every currently live insert on delete.rule_id
        _ = try self.query(
            \\ insert into imp_deletes 
            \\   select ?1, ?2, imp_inserts.tx_id
            \\   from imp_inserts
            \\   where imp_inserts.rule_id = ?2 
            \\   and not exists (
            \\     select * from imp_deletes 
            \\     where imp_inserts.tx_id = imp_deletes.deleted_tx_id
            \\     and imp_inserts.rule_id = imp_deletes.rule_id
            \\   )
            \\ ;
        , &[_]imp.Atom{
            .{ .Number = @intToFloat(f64, delete.tx_id) },
            .{ .Number = @intToFloat(f64, delete.rule_id) },
        });
    for (inserts) |insert|
        _ = try self.query(
            \\ insert into imp_inserts values (?1, ?2, ?3);
        , &[_]imp.Atom{
            .{ .Number = @intToFloat(f64, insert.tx_id) },
            .{ .Number = @intToFloat(f64, insert.rule_id) },
            .{ .Text = insert.rule },
        });
    _ = try self.query(
        \\ commit transaction;
    , &.{});
}

pub fn getLiveInserts(self: *Self) ![]const Insert {
    const rows = try self.query(
        \\ select tx_id, rule_id, rule from imp_inserts where not exists (
        \\   select * from imp_deletes 
        \\   where imp_inserts.tx_id = imp_deletes.deleted_tx_id
        \\   and imp_inserts.rule_id = imp_deletes.rule_id
        \\ );
    , &.{});
    var inserts = u.ArrayList(Insert).init(self.arena.allocator());
    for (rows) |row|
        try inserts.append(.{
            .tx_id = @floatToInt(RuleId, row[0].Number),
            .rule_id = @floatToInt(RuleId, row[1].Number),
            .rule = row[2].Text,
        });
    return inserts.toOwnedSlice();
}

pub fn getLiveSource(self: *Self) ![]const u8 {
    const inserts = try self.getLiveInserts();
    var rules = u.DeepHashMap(imp.Storage.RuleId, []const u8).init(self.arena.allocator());
    for (inserts) |insert| {
        if (try rules.fetchPut(insert.rule_id, insert.rule)) |kv| {
            u.panic("Merge conflict on rule_id {}.\nFirst rule:\n{s}\n\nSecond rule:\n{s}", .{ insert.rule_id, insert.rule, kv.value });
        }
    }
    var source = u.ArrayList(u8).init(self.arena.allocator());
    var rules_iter = rules.iterator();
    while (rules_iter.next()) |entry| {
        try source.appendSlice(entry.value_ptr.*);
        try source.appendSlice("\n\n");
    }
    return source.toOwnedSlice();
}

// ---

fn query(self: *Self, sql: [:0]const u8, sql_params: imp.Row) ![]const imp.Row {
    // Prepare statement.
    var statement: ?*c.sqlite3_stmt = undefined;
    defer _ = c.sqlite3_finalize(statement);
    {
        var remaining_sql: [*c]const u8 = sql;
        const result = c.sqlite3_prepare_v2(
            self.db,
            sql,
            @intCast(c_int, sql.len),
            &statement,
            &remaining_sql,
        );
        check_sqlite_error(self.db, result);
        u.assert(@ptrToInt(remaining_sql) - @ptrToInt(@ptrCast([*c]const u8, sql)) == sql.len);
    }

    // Bind parameters
    for (sql_params) |sql_param, i| {
        const result = switch (sql_param) {
            // NOTE I think it's safe to cast text to `[*c]const u8` because sqlite is supposed to use the length we pass instead of the null terminator
            .Text => |text| c.sqlite3_bind_text(statement.?, @intCast(c_int, i + 1), @ptrCast([*c]const u8, text), @intCast(c_int, text.len), c.SQLITE_STATIC),
            .Number => |number| c.sqlite3_bind_double(statement.?, @intCast(c_int, i + 1), number),
        };
        check_sqlite_error(self.db, result);
    }

    // Read results.
    var rows = std.ArrayList(imp.Row).init(self.arena.allocator());
    while (true) {
        const result = c.sqlite3_step(statement);
        switch (result) {
            c.SQLITE_DONE => break,
            c.SQLITE_ROW => {
                const num_columns = c.sqlite3_column_count(statement);
                var column: c_int = 0;
                var row = std.ArrayList(imp.Atom).init(self.arena.allocator());
                while (column < num_columns) : (column += 1) {
                    const column_type = c.sqlite3_column_type(statement, column);
                    const scalar = switch (column_type) {
                        c.SQLITE_INTEGER => imp.Atom{ .Number = @intToFloat(f64, c.sqlite3_column_int64(statement, column)) },
                        c.SQLITE_FLOAT => imp.Atom{ .Number = c.sqlite3_column_double(statement, column) },
                        c.SQLITE_TEXT => value: {
                            // ptr is stored in statement and freed by sqlite3_finalize
                            const ptr = c.sqlite3_column_text(statement, column);
                            const len = @intCast(usize, c.sqlite3_column_bytes(statement, column));
                            const slice = try self.arena.allocator().dupe(u8, ptr[0..len]);
                            break :value imp.Atom{ .Text = slice };
                        },
                        c.SQLITE_BLOB => u.panic("Sqlite: can't convert value of type BLOB to imp", .{}),
                        c.SQLITE_NULL => u.panic("Sqlite: can't convert value of type NULL to imp", .{}),
                        // No other sqlite types: https://www.sqlite.org/c3ref/c_blob.html
                        else => unreachable,
                    };
                    try row.append(scalar);
                }
                try rows.append(row.toOwnedSlice());
            },
            else => check_sqlite_error(self.db, result),
        }
    }

    return rows.toOwnedSlice();
}

fn check_sqlite_error(db: *c.sqlite3, result: c_int) void {
    if (result != c.SQLITE_OK) {
        u.panic("Sqlite: {s}", .{c.sqlite3_errmsg(db)});
    }
}

// ---

test "storage" {
    var arena = u.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var storage = try Self.init(&arena, ":memory:");
    try u.expectDump(try storage.getLiveInserts(),
        \\lib.imp.util.ViaDump([]const lib.imp.Storage.Insert){
        \\    .thing = []Insert[
        \\    ],
        \\}
    );

    var tx_id: TransactionId = 0;
    try storage.commit(
        &[_]Insert{
            .{ .tx_id = tx_id, .rule_id = 0, .rule = 
            \\parent("Alice", "Bob").
            },
            .{ .tx_id = tx_id, .rule_id = 1, .rule = 
            \\parent("Bob", "Charlene").
            },
        },
        &.{},
    );
    try u.expectDump(try storage.getLiveInserts(),
        \\lib.imp.util.ViaDump([]const lib.imp.Storage.Insert){
        \\    .thing = []Insert[
        \\        Insert{
        \\            .tx_id = 0,
        \\            .rule_id = 0,
        \\            .rule = "parent("Alice", "Bob").",
        \\        },
        \\        Insert{
        \\            .tx_id = 0,
        \\            .rule_id = 1,
        \\            .rule = "parent("Bob", "Charlene").",
        \\        },
        \\    ],
        \\}
    );

    tx_id += 1;
    try storage.commit(
        &[_]Insert{
            .{ .tx_id = tx_id, .rule_id = 1, .rule = 
            \\parent("Bob", "Danny").
            },
        },
        &.{},
    );
    // We don't detect rule conflicts at this level - that's the job of the ide
    try u.expectDump(try storage.getLiveInserts(),
        \\lib.imp.util.ViaDump([]const lib.imp.Storage.Insert){
        \\    .thing = []Insert[
        \\        Insert{
        \\            .tx_id = 0,
        \\            .rule_id = 0,
        \\            .rule = "parent("Alice", "Bob").",
        \\        },
        \\        Insert{
        \\            .tx_id = 0,
        \\            .rule_id = 1,
        \\            .rule = "parent("Bob", "Charlene").",
        \\        },
        \\        Insert{
        \\            .tx_id = 1,
        \\            .rule_id = 1,
        \\            .rule = "parent("Bob", "Danny").",
        \\        },
        \\    ],
        \\}
    );

    tx_id += 1;
    try storage.commit(
        &[_]Insert{
            .{ .tx_id = tx_id, .rule_id = 1, .rule = 
            \\parent("Bob", "Eve").
            },
        },
        &[_]Delete{
            .{ .tx_id = tx_id, .rule_id = 1 },
        },
    );
    try u.expectDump(try storage.getLiveInserts(),
        \\lib.imp.util.ViaDump([]const lib.imp.Storage.Insert){
        \\    .thing = []Insert[
        \\        Insert{
        \\            .tx_id = 0,
        \\            .rule_id = 0,
        \\            .rule = "parent("Alice", "Bob").",
        \\        },
        \\        Insert{
        \\            .tx_id = 2,
        \\            .rule_id = 1,
        \\            .rule = "parent("Bob", "Eve").",
        \\        },
        \\    ],
        \\}
    );
}
