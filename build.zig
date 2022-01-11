const std = @import("std");

pub fn build(b: *std.build.Builder) !void {
    const mode = b.standardReleaseOptions();
    var target = b.standardTargetOptions(.{});
    target.setGnuLibCVersion(2, 28, 0);

    const run = addBin(b, mode, target, "run", "Evaluate an imp file", "./bin/run.zig");
    run.run.addArgs(b.args orelse &.{});

    const test_end_to_end = addBin(b, mode, target, "test_end_to_end", "Run an end-to-end test file", "./test/end_to_end.zig");
    test_end_to_end.run.addArgs(b.args orelse &.{"./test/end_to_end.test"});

    const test_unit_bin = b.addTestExe("test_unit", "lib/imp.zig");
    commonSetup(test_unit_bin, mode, target);
    const test_unit_run = test_unit_bin.run();
    const test_unit_step = b.step("test_unit", "Run unit tests");
    test_unit_step.dependOn(&test_unit_run.step);

    const test_step = b.step("test", "Run all tests");
    // Make sure that run.zig builds
    test_step.dependOn(&run.bin.step);
    test_step.dependOn(test_end_to_end.step);
    test_step.dependOn(test_unit_step);
}

fn addBin(
    b: *std.build.Builder,
    mode: std.builtin.Mode,
    target: std.zig.CrossTarget,
    name: []const u8,
    description: []const u8,
    exe_path: []const u8,
) struct {
    bin: *std.build.LibExeObjStep,
    run: *std.build.RunStep,
    step: *std.build.Step,
} {
    const bin = b.addExecutable(name, exe_path);
    commonSetup(bin, mode, target);
    const run = bin.run();
    const step = b.step(name, description);
    step.dependOn(&run.step);
    return .{ .bin = bin, .run = run, .step = step };
}

fn commonSetup(
    bin: *std.build.LibExeObjStep,
    mode: std.builtin.Mode,
    target: std.zig.CrossTarget,
) void {
    bin.setMainPkgPath("./");
    bin.linkLibC();
    bin.addIncludeDir("deps/sqlite-amalgamation-3370000/");
    bin.addCSourceFile("deps/sqlite-amalgamation-3370000/sqlite3.c", &[_][]const u8{"-std=c99"});
    bin.setBuildMode(mode);
    bin.setTarget(target);
}
