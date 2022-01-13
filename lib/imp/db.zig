//! An imp program proposes a diff that looks like:
//!     :insert, row...
//!     :delete, row...
//! We generate a random transaction id.
//! Then for each delete, we include the transaction ids which inserted any copies of that row that currently exist.
//! The actual transaction then looks like:
//!     tid, :insert, row...
//!     tid, :delete, row..., insert_tid
//!     tid, :delete, row..., insert_tid
//! The whole transaction is serialized to an imp program and stored in a sqlite row.

const std = @import("std");
const imp = @import("../imp.zig");
const u = imp.util;
const syntax = imp.lang.repr.syntax;
const value = imp.lang.repr.value;

const c = @cImport({
    @cInclude("sqlite3.h");
});

pub const DB = struct {
    allocator: u.Allocator,
    arena: *u.ArenaAllocator,
    db: *c.sqlite3,
    tid_gen: TidGen,
    transactions: value.Set,
    tids_and_rows: value.Set,
    rows: value.Set,
    error_info: ?imp.lang.ErrorInfo,

    const TidGen = union(enum) {
        Real: std.rand.DefaultCsprng,
        Testing: *u52,
    };

    pub fn init(allocator: u.Allocator, db_path: [:0]const u8) !DB {
        const arena = try allocator.create(u.ArenaAllocator);
        arena.* = u.ArenaAllocator.init(allocator);
        const empty_set = value.Set{ .rows = u.DeepHashSet(value.Row).init(arena.allocator()) };
        var seed: [32]u8 = undefined;
        try std.os.getrandom(&seed);
        return DB{
            .allocator = allocator,
            .arena = arena,
            .tid_gen = .{ .Real = std.rand.DefaultCsprng.init(seed) },
            .db = try open(arena, db_path),
            .transactions = empty_set,
            .tids_and_rows = empty_set,
            .rows = empty_set,
            .error_info = null,
        };
    }

    pub fn deinit(self: DB) void {
        close(self.db);
        self.arena.deinit();
        self.allocator.destroy(self.arena);
    }

    pub fn applyDiff(self: *DB, diff: value.Set) !void {
        const tid = switch (self.tid_gen) {
            .Real => |*rng| rng.random().int(u52),
            .Testing => |next_tid| tid: {
                next_tid.* += 1;
                break :tid next_tid.* - 1;
            },
        };
        const transaction = try diffToTransaction(self.arena, self.tids_and_rows, tid, diff);
        try putTransaction(self.arena, self.db, transaction);
        self.transactions = try getTransactions(self.arena, self.db);
        self.tids_and_rows = try transactionsToTidAndRows(self.arena, self.transactions);
        self.rows = try tidAndRowsToRows(self.arena, self.tids_and_rows);
    }

    pub fn syncFrom(self: *DB, other: DB) !void {
        try putTransaction(self.arena, self.db, other.transactions);
        self.transactions = try getTransactions(self.arena, self.db);
        self.tids_and_rows = try transactionsToTidAndRows(self.arena, self.transactions);
        self.rows = try tidAndRowsToRows(self.arena, self.tids_and_rows);
    }

    // --- for testing ---

    pub fn initTesting(next_tid: *u52) !DB {
        const arena = try std.testing.allocator.create(u.ArenaAllocator);
        arena.* = u.ArenaAllocator.init(std.testing.allocator);
        const empty_set = value.Set{ .rows = u.DeepHashSet(value.Row).init(arena.allocator()) };
        return DB{
            .allocator = std.testing.allocator,
            .arena = arena,
            .tid_gen = .{ .Testing = next_tid },
            .db = try open(arena, ":memory:"),
            .transactions = empty_set,
            .tids_and_rows = empty_set,
            .rows = empty_set,
            .error_info = null,
        };
    }

    pub fn applyDiffFromSource(self: *DB, diff_source: []const u8) !void {
        const constants = u.DeepHashMap(syntax.Name, value.Set).init(self.arena.allocator());
        const diff = try imp.lang.eval(self.arena, diff_source, constants, &self.error_info);
        try self.applyDiff(diff);
    }

    pub fn expectEqual(self: *DB, string: []const u8) !void {
        const self_string = try u.formatToString(self.arena.allocator(), "{}", .{self.tids_and_rows});
        try std.testing.expectEqualStrings(string, self_string);
    }
};

fn open(arena: *u.ArenaAllocator, db_path: [:0]const u8) !*c.sqlite3 {
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
    check_sqlite_error(db, result);

    // Ensure that transactions table exists.
    _ = try query(arena, db.?,
        \\ create table if not exists imp_transactions(data text);
    , &.{});

    return db.?;
}

fn close(db: *c.sqlite3) void {
    _ = c.sqlite3_close(db);
}

fn diffToTransaction(arena: *u.ArenaAllocator, tids_and_rows: value.Set, tid: u52, diff: value.Set) !value.Set {
    var transaction_rows = u.DeepHashSet(value.Row).init(arena.allocator());

    // Handle inserts. Build an index of deletes.
    var deletes = u.DeepHashSet(value.Row).init(arena.allocator());
    {
        var iter = diff.rows.keyIterator();
        while (iter.next()) |diff_row| {
            if (u.deepEqual(diff_row.*[0], .{ .TextTag = "insert" }))
                try transaction_rows.put(
                    try std.mem.concat(arena.allocator(), value.Scalar, &.{
                        // tid
                        &[_]value.Scalar{.{ .Number = @intToFloat(f64, tid) }},
                        // :insert, row...
                        diff_row.*,
                    }),
                    {},
                );
            if (u.deepEqual(diff_row.*[0], .{ .TextTag = "delete" }))
                try deletes.put(diff_row.*[1..], {});
        }
    }

    // Handle deletes by looking up matching rows in current data.
    {
        var iter = tids_and_rows.rows.keyIterator();
        while (iter.next()) |tid_and_row|
            if (deletes.contains(tid_and_row.*[1..]))
                try transaction_rows.put(
                    try std.mem.concat(arena.allocator(), value.Scalar, &.{
                        &[_]value.Scalar{
                            // tid
                            .{ .Number = @intToFloat(f64, tid) },
                            // :delete
                            .{ .TextTag = try arena.allocator().dupe(u8, "delete") },
                        },
                        // row...
                        tid_and_row.*[1..],
                        &[_]value.Scalar{
                            // insert_tid
                            .{ .Number = tid_and_row.*[0].Number },
                        },
                    }),
                    {},
                );
    }

    return value.Set{ .rows = transaction_rows };
}

fn putTransaction(arena: *u.ArenaAllocator, db: *c.sqlite3, transaction: value.Set) !void {
    // Convert transaction to string.
    const transaction_string = try u.formatToString(arena.allocator(), "{}", .{transaction});

    // Insert transaction.
    _ = try query(arena, db,
        \\ insert into imp_transactions(data) values (?);
    , &[_]value.Scalar{.{ .Text = transaction_string }});
}

fn getTransactions(arena: *u.ArenaAllocator, db: *c.sqlite3) !value.Set {
    // Get transactions from sqlite.
    const serialized_transactions = try query(arena, db,
        \\ select data from imp_transactions;
    , &.{});

    // Convert transactions to imp.
    var transactions = u.DeepHashSet(value.Row).init(arena.allocator());
    for (serialized_transactions) |serialized_transaction| {
        const constants = u.DeepHashMap(syntax.Name, value.Set).init(arena.allocator());
        var error_info: ?imp.lang.ErrorInfo = null;
        if (imp.lang.eval(arena, serialized_transaction[0].Text, constants, &error_info)) |transaction| {
            var iter = transaction.rows.keyIterator();
            while (iter.next()) |row|
                try transactions.put(row.*, {});
        } else |_| {
            u.panic("Loading transactions from sqlite: {}", .{error_info});
        }
    }

    return value.Set{ .rows = transactions };
}

fn transactionsToTidAndRows(arena: *u.ArenaAllocator, transactions: value.Set) !value.Set {
    var rows = u.DeepHashSet(value.Row).init(arena.allocator());

    // Handle inserts.
    var iter = transactions.rows.keyIterator();
    while (iter.next()) |transaction|
        if (u.deepEqual(transaction.*[1], .{ .TextTag = "insert" }))
            try rows.put(
                try std.mem.concat(arena.allocator(), value.Scalar, &.{
                    // tid
                    &[_]value.Scalar{transaction.*[0]},
                    // row...
                    transaction.*[2..],
                }),
                {},
            );

    // Handle deletes.
    iter = transactions.rows.keyIterator();
    while (iter.next()) |transaction| {
        if (u.deepEqual(transaction.*[1], .{ .TextTag = "delete" })) {
            _ = rows.remove(
                try std.mem.concat(arena.allocator(), value.Scalar, &.{
                    // insert_tid
                    &[_]value.Scalar{transaction.*[transaction.len - 1]},
                    // row...
                    transaction.*[2 .. transaction.len - 1],
                }),
            );
        }
    }

    return value.Set{ .rows = rows };
}

fn tidAndRowsToRows(arena: *u.ArenaAllocator, tid_and_rows: value.Set) !value.Set {
    // Just have to discard the tid in column 0.
    var rows = u.DeepHashSet(value.Row).init(arena.allocator());
    var iter = tid_and_rows.rows.keyIterator();
    while (iter.next()) |tid_and_row|
        try rows.put(try arena.allocator().dupe(value.Scalar, tid_and_row.*[1..]), {});
    return value.Set{ .rows = rows };
}

fn query(arena: *u.ArenaAllocator, db: *c.sqlite3, sql: [:0]const u8, sql_params: value.Row) ![]const value.Row {
    // Prepare statement.
    var statement: ?*c.sqlite3_stmt = undefined;
    defer _ = c.sqlite3_finalize(statement);
    {
        var remaining_sql: [*c]const u8 = sql;
        const result = c.sqlite3_prepare_v2(
            db,
            sql,
            @intCast(c_int, sql.len),
            &statement,
            &remaining_sql,
        );
        check_sqlite_error(db, result);
        u.assert(@ptrToInt(remaining_sql) - @ptrToInt(@ptrCast([*c]const u8, sql)) == sql.len);
    }

    // Bind parameters
    for (sql_params) |sql_param, i| {
        const result = switch (sql_param) {
            // NOTE I think it's safe to case text to `[*c]const u8` because sqlite is supposed to use the length we pass
            .Text => |text| c.sqlite3_bind_text(statement.?, @intCast(c_int, i + 1), @ptrCast([*c]const u8, text), @intCast(c_int, text.len), c.SQLITE_STATIC),
            .Number => |number| c.sqlite3_bind_double(statement.?, @intCast(c_int, i + 1), number),
            else => u.panic("Sqlite: can't convert value {} to sqlite", .{sql_param}),
        };
        check_sqlite_error(db, result);
    }

    // Read results.
    var rows = std.ArrayList(value.Row).init(arena.allocator());
    while (true) {
        const result = c.sqlite3_step(statement);
        switch (result) {
            c.SQLITE_DONE => break,
            c.SQLITE_ROW => {
                const num_columns = c.sqlite3_column_count(statement);
                var column: c_int = 0;
                var row = std.ArrayList(value.Scalar).init(arena.allocator());
                while (column < num_columns) : (column += 1) {
                    const column_type = c.sqlite3_column_type(statement, column);
                    const scalar = switch (column_type) {
                        c.SQLITE_INTEGER => value.Scalar{ .Number = @intToFloat(f64, c.sqlite3_column_int64(statement, column)) },
                        c.SQLITE_FLOAT => value.Scalar{ .Number = c.sqlite3_column_double(statement, column) },
                        c.SQLITE_TEXT => value: {
                            // ptr is stored in statement and freed by sqlite3_finalize
                            const ptr = c.sqlite3_column_text(statement, column);
                            const len = @intCast(usize, c.sqlite3_column_bytes(statement, column));
                            const slice = try arena.allocator().dupe(u8, ptr[0..len]);
                            break :value value.Scalar{ .Text = slice };
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
            else => check_sqlite_error(db, result),
        }
    }

    return rows.toOwnedSlice();
}

fn check_sqlite_error(db: ?*c.sqlite3, result: c_int) void {
    if (result != c.SQLITE_OK) {
        u.panic("Sqlite: {s}", .{c.sqlite3_errmsg(db)});
    }
}

test "sqlite diffs" {
    var next_tid: u52 = 0;
    var alice = try DB.initTesting(&next_tid);
    defer alice.deinit();

    // basic insert
    try alice.applyDiffFromSource(
        \\| :insert, :foo, 42
        \\| :insert, :bar, "a", "b"
    );
    try alice.expectEqual(
        \\| 0, :"foo", 42
        \\| 0, :"bar", "a", "b"
    );

    // delete something that exists - it goes away
    try alice.applyDiffFromSource(
        \\| :delete, :foo, 42
    );
    try alice.expectEqual(
        \\| 0, :"bar", "a", "b"
    );

    // delete something that doesn't exist - noop
    try alice.applyDiffFromSource(
        \\| :delete, :bar, "a", "not b"
    );
    try alice.expectEqual(
        \\| 0, :"bar", "a", "b"
    );

    // insert something that was previously deleted - works
    try alice.applyDiffFromSource(
        \\| :insert, :bar, "a", "not b"
    );
    try alice.expectEqual(
        \\| 0, :"bar", "a", "b"
        \\| 3, :"bar", "a", "not b"
    );
}

test "sqlite transactions" {
    var next_tid: u52 = 0;
    var alice = try DB.initTesting(&next_tid);
    defer alice.deinit();
    var bob = try DB.initTesting(&next_tid);
    defer bob.deinit();

    // merging transactions from peer - order doesn't matter
    try alice.applyDiffFromSource(
        \\| :insert, :quux
    );
    try alice.applyDiffFromSource(
        \\| :delete, :quux
    );
    try bob.syncFrom(alice);
    try bob.expectEqual(
        \\none
    );

    // deletes from peers only affect inserts from peers that were visible on that peer
    try bob.applyDiffFromSource(
        \\| :insert, :yo
    );
    try bob.applyDiffFromSource(
        \\| :insert, :yo
    );
    try bob.expectEqual(
        \\| 2, :"yo"
        \\| 3, :"yo"
    );
    try alice.syncFrom(bob);
    try alice.applyDiffFromSource(
        \\| :delete, :yo
    );
    try bob.applyDiffFromSource(
        \\| :insert, :yo
    );
    try bob.syncFrom(alice);
    try bob.expectEqual(
        \\| 5, :"yo"
    );
}
