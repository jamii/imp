const imp = @import("../lib/imp.zig");
usingnamespace imp.common;

fn fuzz_panic() noreturn {
    panic("Internal error in fuzzer", .{});
}

const Input = struct {
    allocator: *Allocator,
    arena: *ArenaAllocator,
    bytes: []const u8,
    i: usize,
    closed_ranges: ArrayList([2]usize), // [lo, hi)
    open_ranges: ArrayList(usize),

    fn init(allocator: *Allocator, bytes: []const u8) Input {
        var arena = allocator.create(ArenaAllocator) catch fuzz_panic();
        arena.* = ArenaAllocator.init(allocator);
        return Input{
            .allocator = allocator,
            .arena = arena,
            .bytes = bytes,
            .i = 0,
            .closed_ranges = ArrayList([2]usize).init(allocator),
            .open_ranges = ArrayList(usize).init(allocator),
        };
    }

    fn deinit(self: *Input) void {
        self.arena.deinit();
        self.allocator.destroy(self.arena);
        self.closed_ranges.deinit();
        self.open_ranges.deinit();
    }

    fn getBytes(self: *Input, comptime len: usize) [len]u8 {
        var bytes: [len]u8 = undefined;
        var i: usize = 0;
        while (i < len) : ({i += 1; self.i += 1;}) {
            if (self.i < self.bytes.len) {
                bytes[i] = self.bytes[self.i];
            } else {
                bytes[i] = 0;
            }
        }
        return bytes;
    }

    fn startRange(self: *Input) void {
        self.open_ranges.append(self.i) catch fuzz_panic();
    }

    fn endRange(self: *Input) void {
        const lo = self.open_ranges.pop();
        const hi = min(self.i, self.bytes.len);
        if (lo < self.bytes.len) {
            self.closed_ranges.append(.{lo, hi}) catch fuzz_panic();
        }
    }
};

fn Int(comptime T: type) type {
    return struct {
        fn generate(input: *Input) T {
            input.startRange();
            // this is copied from std.rand.Random.int
            const UnsignedT = std.meta.IntType(false, T.bit_count);
            const ByteAlignedT = std.meta.IntType(false, @divTrunc(T.bit_count + 7, 8) * 8);
            const bytes = input.getBytes(@sizeOf(ByteAlignedT));
            const byte_aligned_result = std.mem.readIntSliceLittle(ByteAlignedT, &bytes);
            const unsigned_result = @truncate(UnsignedT, byte_aligned_result);
            const value = @bitCast(T, unsigned_result);
            input.endRange();
            return value;
        }
    };
}

const Bool = struct {
    fn generate(input: *Input) bool {
        return Int(u1).generate(input) == 1;
    }
};

fn Slice(comptime Elem: type, comptime min_len: usize) type {
    const T = std.meta.declarationInfo(Elem, "generate").data.Fn.return_type;
    return struct {
        fn generate(input: *Input) []T {
            var output = ArrayList(T).init(&input.arena.allocator);
            input.startRange();
            while (true) {
                input.startRange();
                defer input.endRange();
                if (output.items.len > min_len and !Bool.generate(input)) break;
                output.append(Elem.generate(input)) catch fuzz_panic();
            }
            input.endRange();
            return output.items;
        }
    };
}

fn Ascii(comptime min_len: usize) type {
    return struct {
        fn generate(input: *Input) []u8 {
            var output = ArrayList(u8).init(&input.arena.allocator);
            const bytes = Slice(Int(u7), min_len).generate(input);
            for (bytes) |byte| {
                output.append(byte) catch fuzz_panic();
            }
            return output.items;
        }
    };
}

fn Utf8(comptime min_len: usize) type {
    return struct {
        fn generate(input: *Input) []u8 {
            var output = ArrayList(u8).init(&input.arena.allocator);
            input.startRange();
            while (true) {
                input.startRange();
                defer input.endRange();
                if (output.items.len > min_len and !Bool.generate(input)) break;
                while (true) {
                    // 0 is a valid char so this won't loop forever
                    const char = Int(u21).generate(input);
                    var bytes = [4]u8{0,0,0,0};
                    if (std.unicode.utf8Encode(char, &bytes)) |len| {
                        output.appendSlice(bytes[0..len]) catch fuzz_panic();
                        break;
                    } else |_| {
                        // ignore this char
                    }
                }
            }
            input.endRange();
            return output.items;
        }
    };
}

fn fuzz(allocator: *Allocator, seed: u64, iterations: usize, test_fn: fn(*Input) anyerror!void) void {
    var rng = std.rand.DefaultPrng.init(seed);
    var random = &rng.random;
    var iteration: usize = 0;
    while (iteration < iterations) : (iteration += 1) {

        // generate some random bytes
        var bytes = ArrayList(u8).init(allocator);
        var num_bytes = random.uintLessThan(usize, iteration + 1);
        while (num_bytes > 0) : (num_bytes -= 1) {
            bytes.append(random.int(u8)) catch fuzz_panic();
        }

        // test
        // TODO catch panic somehow?
        var input = Input.init(allocator, bytes.items);
        const result = test_fn(&input);
        assert(input.open_ranges.items.len == 0);
        if (result) |_| {
            bytes.deinit();
            input.deinit();
        } else |err| {
            warn("\nIteration {} raised error {}:\n{}\n", .{iteration, err, bytes.items});

            var most_shrunk_bytes = bytes;
            var most_shrunk_input = input;
            var most_shrunk_err = err;
            var shrink_iteration: usize = 0;
            while (shrink_iteration < iterations and most_shrunk_bytes.items.len > 0) : (shrink_iteration += 1) {
                // copy most_shrunk_bytes
                var shrunk_bytes = ArrayList(u8).initCapacity(allocator, most_shrunk_bytes.items.len) catch fuzz_panic();
                shrunk_bytes.appendSlice(most_shrunk_bytes.items) catch fuzz_panic();

                // decide how to shrink
                if (random.boolean()) {
                    // shrink a byte
                    const i = random.uintLessThan(usize, shrunk_bytes.items.len);
                    shrunk_bytes.items[i] = random.uintLessThan(u8, max(shrunk_bytes.items[i], 1));
                } else {
                    // remove a range
                    const range_ix = random.uintLessThan(usize, most_shrunk_input.closed_ranges.items.len);
                    const range = most_shrunk_input.closed_ranges.items[range_ix];
                    var i = range[1];
                    while (i < shrunk_bytes.items.len) : (i += 1) {
                        shrunk_bytes.items[i - (range[1] - range[0])] = shrunk_bytes.items[i];
                    }
                    shrunk_bytes.shrink(shrunk_bytes.items.len - (range[1] - range[0]));
                }

                // test again
                var shrunk_input = Input.init(allocator, shrunk_bytes.items);
                const shrunk_result = test_fn(&shrunk_input);
                assert(input.open_ranges.items.len == 0);
                if (shrunk_result) |_| {
                    shrunk_bytes.deinit();
                    shrunk_input.deinit();
                } else |shrunk_err| {
                    most_shrunk_bytes.deinit();
                    most_shrunk_input.deinit();
                    most_shrunk_bytes = shrunk_bytes;
                    most_shrunk_input = shrunk_input;
                    most_shrunk_err = shrunk_err;
                }
            }

            warn("\nShrunk to {}:\n{}\n", .{most_shrunk_err, most_shrunk_bytes.items});
            most_shrunk_bytes.deinit();
            most_shrunk_input.deinit();
            return;
        }
    }
}

fn no_fo(input: *Input) !void {
    const string = Ascii(1).generate(input);
    if (std.mem.indexOf(u8, string, "fo")) |_| {
        return error.Fail;
    }
}

test "no_fo" {
    fuzz(std.heap.page_allocator, 40, 100_000, no_fo);
}
