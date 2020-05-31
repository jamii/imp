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

fn generatorReturnType(comptime gen: type) type {
    return std.meta.declarationInfo(gen, "generate").data.Fn.return_type;
}

const Void = struct {
    fn generate(input: *Input) void {
        return;
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
            const output = @bitCast(T, unsigned_result);
            input.endRange();
            return output;
        }
    };
}

fn Float(comptime T: type) type {
    return struct {
        fn generate(input: *Input) T {
            input.startRange();
            // this is copied from std.rand.Random.float
            const output = output: {
                switch (T) {
                    f32 => {
                        const s = Int(u32).generate(input);
                        const repr = (0x7f << 23) | (s >> 9);
                        break :output @bitCast(f32, repr) - 1.0;
                    },
                    f64 => {
                        const s = Int(u64).generate(input);
                        const repr = (0x3ff << 52) | (s >> 12);
                        break :output @bitCast(f64, repr) - 1.0;
                    },
                    else => @compileError("unknown floating point type"),
                }
            };
            input.endRange();
            return output;
        }
    };
}

const Bool = struct {
    fn generate(input: *Input) bool {
        return Int(u1).generate(input) == 1;
    }
};

fn Slice(comptime elem_gen: type, comptime min_len: usize) type {
    const T = generatorReturnType(elem_gen);
    return struct {
        fn generate(input: *Input) []T {
            var output = ArrayList(T).init(&input.arena.allocator);
            input.startRange();
            while (true) {
                input.startRange();
                defer input.endRange();
                if (output.items.len > min_len and !Bool.generate(input)) break;
                output.append(elem_gen.generate(input)) catch fuzz_panic();
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

fn Struct(comptime T: type, comptime field_gens: var) type {
    return struct {
        fn generate(input: *Input) T {
            input.startRange();
            var output: T = undefined;
            inline for (@typeInfo(T).Struct.fields) |field| {
                @field(output, field.name) = @field(field_gens, field.name).generate(input);
            }
            input.endRange();
            return output;
        }
    };
}

fn Union(comptime T: type, comptime field_gens: var) type {
    return struct {
        fn generate(input: *Input) T {
            input.startRange();
            var output: T = undefined;
            const field_ix = min(Int(u8).generate(input), @typeInfo(T).Union.fields.len-1);
            inline for (@typeInfo(T).Union.fields) |field, i| {
                if (field_ix == i) {
                    output = @unionInit(T, field.name, @field(field_gens, field.name).generate(input));
                }
            }
            input.endRange();
            return output;
        }
    };
}

fn Ptr(comptime gen: var) type {
    const T = generatorReturnType(gen);
    return struct {
        fn generate(input: *Input) T {
            var output = input.arena.allocator.create(T);
            const value = gen.generate(input);
            output.* = value;
        }
    };
}

const Scalar = struct {
    fn generate(input: *Input) imp.lang.repr.value.Scalar {
        if (Bool.generate(input)) {
            return .{.String = Utf8(0).generate(input)};
        } else {
            return .{.Number = Float(f64).generate(input)};
        }
        // never generate Box - it's not a valid syntax literal
    }
};

const Name = Ascii(1);

const Syntax = struct {

    const Pair = Struct(
        imp.lang.repr.syntax.Pair,
        .{
            .left = PtrExpr,
            .right = PtrExpr,
        }
    );

    const When = Struct(
        imp.lang.repr.syntax.When,
        .{
            .condition = PtrExpr,
            .true_branch = PtrExpr,
        }
    );

    const Abstract = Struct(
        imp.lang.repr.syntax.Abstract,
        .{
            .args = Slice(Arg, 1),
            .body = PtrExpr,
        }
    );

    const Arg = Struct(
        imp.lang.repr.syntax.Arg,
        .{
            .name = Name,
            .unbox = Bool,
        }
    );

    const Annotate = Struct(
        imp.lang.repr.syntax.Annotate,
        .{
            .annotation = Name,
            .body = PtrExpr,
        }
    );

    const If = Struct(
        imp.lang.repr.syntax.If,
        .{
            .condition = PtrExpr,
            .true_branch = PtrExpr,
            .false_branch = PtrExpr,
        }
    );

    const Let = Struct(
        imp.lang.repr.syntax.Let,
        .{
            .name = Name,
            .value = PtrExpr,
            .body = PtrExpr,
        }
    );

    const Lookup = Struct(
        imp.lang.repr.syntax.Lookup,
        .{
            .value = PtrExpr,
            .name = Name,
        }
    );

    const Expr = Union(
        imp.lang.repr.syntax.Expr,
        .{
            .None = Void,
            .Some = Void,
            .Scalar = Scalar,
            .Union = Pair,
            .Intersect = Pair,
            .Product = Pair,
            .Equal = Pair,
            .Name = Name,
            .When = When,
            .Abstract = Abstract,
            .Apply = Pair,
            .Box = PtrExpr,
            .Annotate = Annotate,

            .Negate = PtrExpr,
            .If = If,
            .Let = Let,
            .Lookup = Lookup,
        }
    );

    const PtrExpr = struct {
        fn generate(input: *Input) *const imp.lang.repr.syntax.Expr {
            var output = input.arena.allocator.create(imp.lang.repr.syntax.Expr) catch fuzz_panic();
            output.* = Expr.generate(input);
            return output;
        }
    };
};

const Core = struct {

    const Pair = Struct(
        imp.lang.repr.core.Pair,
        .{
            .left = PtrExpr,
            .right = PtrExpr,
        }
    );

    // will fix these up in makeValid
    const NameIx = Int(usize);

    const When = Struct(
        imp.lang.repr.core.When,
        .{
            .condition = PtrExpr,
            .true_branch = PtrExpr,
        }
    );

    const Box = Struct(
        imp.lang.repr.core.Box,
        .{
            .body = PtrExpr,
            // will fix this up in makeValid
            .scope = Slice(NameIx, 0),
        }
    );

    const Annotate = Struct(
        imp.lang.repr.core.Annotate,
        .{
            .annotation = Name,
            .body = PtrExpr,
        }
    );

    const Expr = Union(
        imp.lang.repr.core.Expr,
        .{
            .None = Void,
            .Some = Void,
            .Scalar = Scalar,
            .Union = Pair,
            .Intersect = Pair,
            .Product = Pair,
            .Equal = Pair,
            .Name = NameIx,
            .UnboxName = NameIx,
            .When = When,
            .Abstract = PtrExpr,
            .Apply = Pair,
            .Box = Box,
            .Annotate = Annotate,
        }
    );

    const PtrExpr = struct {
        fn generate(input: *Input) *const imp.lang.repr.core.Expr {
            var output = input.arena.allocator.create(imp.lang.repr.core.Expr) catch fuzz_panic();
            output.* = Expr.generate(input);
            return output;
        }
    };

    fn makeValid(input: *Input, invalid_expr: *const imp.lang.repr.core.Expr, scope_size: usize) *const imp.lang.repr.core.Expr {
        var valid_expr = invalid_expr.*;
        switch (valid_expr) {
            .Name, .UnboxName => |*name_ix| {
                // pick a valid name_ix
                name_ix.* %= scope_size;
            },
            .Box => |*box| {
                // set the scope that would be produced by Desugarer.desugarBox
                var scope = input.arena.allocator.alloc(imp.lang.repr.core.NameIx, scope_size) catch fuzz_panic();
                for (scope) |*name_ix, i| {
                    name_ix.* = i;
                }
                box.scope = scope;
            },
            else => {},
        }
        const child_scope_size = if (valid_expr == .Abstract) scope_size + 1 else scope_size;
        for (valid_expr.getChildrenMut().slice()) |child| {
            child.* = makeValid(input, child.*, child_scope_size);
        }
        var valid_expr_ptr = input.arena.allocator.create(imp.lang.repr.core.Expr) catch fuzz_panic();
        valid_expr_ptr.* = valid_expr;
        return valid_expr_ptr;
    }

    const ValidPtrExpr = struct {
        fn generate(input: *Input) *const imp.lang.repr.core.Expr {
            const invalid_expr = PtrExpr.generate(input);
            return makeValid(input, invalid_expr, 0);
        }
    };
};

const Options = struct {
    seed: u64,
    fuzz_iterations: usize,
    shrink_iterations: usize,

    const default = Options{
        .seed=42,
        .fuzz_iterations=10_000,
        .shrink_iterations=10_000
    };
};

fn fuzz(allocator: *Allocator, options: Options, test_fn: fn(*Input) anyerror!void) void {
    var rng = std.rand.DefaultPrng.init(options.seed);
    var random = &rng.random;
    var fuzz_iteration: usize = 0;
    while (fuzz_iteration < options.fuzz_iterations) : (fuzz_iteration += 1) {
        warn("Iteration {}\n", .{fuzz_iteration});
        // generate some random bytes
        var bytes = ArrayList(u8).init(allocator);
        var num_bytes = random.uintLessThan(usize, fuzz_iteration + 1);
        while (num_bytes > 0) : (num_bytes -= 1) {
            bytes.append(random.int(u8)) catch fuzz_panic();
        }

        // test
        // TODO catch panic somehow?
        var input = Input.init(allocator, bytes.items);
        warn("Testing\n", .{});
        const result = test_fn(&input);
        warn("Tested\n", .{});
        assert(input.open_ranges.items.len == 0);
        if (result) |_| {
            bytes.deinit();
            input.deinit();
        } else |err| {
            warn("\nIteration {} raised error {}:\n{}\n", .{fuzz_iteration, err, bytes.items});

            var most_shrunk_bytes = bytes;
            var most_shrunk_input = input;
            var most_shrunk_err = err;
            var shrink_iteration: usize = 0;
            while (shrink_iteration < options.shrink_iterations and most_shrunk_bytes.items.len > 0) : (shrink_iteration += 1) {
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

test "fuzz parse" {
    fuzz(std.heap.c_allocator, Options.default, fuzz_parse);
}
fn fuzz_parse(input: *Input) !void {
    const source = Utf8(0).generate(input);
    var store = imp.lang.store.Store.init(input.arena);
    var error_info: ?imp.lang.pass.parse.ErrorInfo = null;
    // should never panic on any input
    _ = imp.lang.pass.parse.parse(&store, source, &error_info) catch |err| {
        switch (err) {
            error.ParseError => return,
            // shouldn't oom unless you're running this on a potato
            error.OutOfMemory => return err,
            // should only return utf8 errors if input is invalid utf8
            error.Utf8InvalidStartByte,
            error.InvalidUtf8,
            error.InvalidCharacter,
            error.Utf8ExpectedContinuation,
            error.Utf8OverlongEncoding,
            error.Utf8EncodesSurrogateHalf,
            error.Utf8CodepointTooLarge => {
                if (std.unicode.utf8ValidateSlice(source)) {
                    return err;
                }
            }
        }
    };
}

test "fuzz desugar" {
    fuzz(std.heap.c_allocator, Options.default, fuzz_desugar);
}
fn fuzz_desugar(input: *Input) !void {
    const expr = Syntax.PtrExpr.generate(input);
    var store = imp.lang.store.Store.init(input.arena);
    var error_info: ?imp.lang.pass.desugar.ErrorInfo = null;
    // should never panic on any input
    _ = imp.lang.pass.desugar.desugar(&store, expr, &error_info) catch |err| {
        switch (err) {
            error.DesugarError => {},
            // shouldn't oom unless you're running this on a potato
            error.OutOfMemory => return err,
        }
    };
}

test "fuzz analyze" {
    fuzz(std.heap.c_allocator, Options.default, fuzz_analyze);
}
fn fuzz_analyze(input: *Input) !void {
    const expr = Core.ValidPtrExpr.generate(input);
    var store = imp.lang.store.Store.init(input.arena);
    var error_info: ?imp.lang.pass.analyze.ErrorInfo = null;
    // should never panic on any input
    _ = imp.lang.pass.analyze.analyze(&store, expr, &error_info) catch |err| {
        switch (err) {
            error.AnalyzeError => {},
            // shouldn't oom unless you're running this on a potato
            // TODO might be able to write programs that require exponential specialization
            error.OutOfMemory => return err,
        }
    };
}

test "fuzz interpret" {
    fuzz(std.heap.c_allocator, Options.default, fuzz_interpret);
}
fn fuzz_interpret(input: *Input) !void {
    const expr = Core.ValidPtrExpr.generate(input);
    var store = imp.lang.store.Store.init(input.arena);
    var analyze_error_info: ?imp.lang.pass.analyze.ErrorInfo = null;
    const analyzed = imp.lang.pass.analyze.analyze(&store, expr, &analyze_error_info);
    var error_info: ?imp.lang.pass.interpret.ErrorInfo = null;
    // should never panic on any valid input
    if (imp.lang.pass.interpret.interpret(&store, input.arena, expr, &error_info)) |set| {
        // if type is finite, should return finite set
        if (analyzed) |set_type| {
            if (set_type == .Finite and set != .Finite) {
                return error.InterpretTooLazy;
            }
        } else |_| {}
    } else |err| {
        switch (err) {
            error.InterpretError => {
                // should never return InterpretError on programs which typecheck
                if (analyzed) |_| {
                    return error.InvalidInterpretError;
                } else |_| {}
            },
            // can easily oom with small programs
            error.OutOfMemory => {},
        }
    }
}

// TODO for some reason the fuzzer OOMs when using std.testing.allocator

// TODO is PtrExpr going to break because it's not in the store?
// could pass Store as var arg to generate

// fn no_fo(input: *Input) !void {
//     const string = Ascii(1).generate(input);
//     if (std.mem.indexOf(u8, string, "fo")) |_| {
//         return error.Fail;
//     }
// }

// test "no_fo" {
//     fuzz(std.heap.page_allocator, Options.default, no_fo);
// }

// export fn LLVMFuzzerTestOneInput(data: [*c]u8, len: size_t) c_int {
//     const bytes = data[0..len];
//     var input = Input.init(std.heap.c_allocator, bytes);
//     no_fo(&input) catch |err| {
//         panic("Test failed with {}", err); // TODO trace
//     };
// }
