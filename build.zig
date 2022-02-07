const std = @import("std");
const zt = @import("deps/ZT/build.zig");

pub fn build(b: *std.build.Builder) !void {
    const mode = b.standardReleaseOptions();
    var target = b.standardTargetOptions(.{});
    target.setGnuLibCVersion(2, 28, 0);

    const run = addBin(b, mode, target, "run", "Evaluate an imp file", "./bin/run.zig");
    run.run.addArgs(b.args orelse &.{});

    const test_unit_bin = b.addTestExe("test_unit", "lib/imp.zig");
    commonSetup(test_unit_bin, mode, target);
    const test_unit_run = test_unit_bin.run();
    const test_unit_step = b.step("test_unit", "Run unit tests");
    test_unit_step.dependOn(&test_unit_run.step);

    const test_step = b.step("test", "Run all tests");
    test_step.dependOn(test_unit_step);
    // Make sure that run.zig builds
    test_step.dependOn(&run.bin.step);
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
    addDeps(bin);
    bin.setBuildMode(mode);
    bin.setTarget(target);
    zt.link(bin);
}

pub fn addDeps(
    bin: *std.build.LibExeObjStep,
) void {
    bin.linkLibC();
    bin.addIncludeDir(getRelativePath() ++ "deps/sqlite-amalgamation-3370000/");
    bin.addCSourceFile(getRelativePath() ++ "deps/sqlite-amalgamation-3370000/sqlite3.c", &[_][]const u8{"-std=c99"});
}

fn getRelativePath() []const u8 {
    comptime var src: std.builtin.SourceLocation = @src();
    return std.fs.path.dirname(src.file).? ++ std.fs.path.sep_str;
}
