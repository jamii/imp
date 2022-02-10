const std = @import("std");
const zt = @import("deps/ZT/build.zig");

pub fn build(b: *std.build.Builder) !void {
    const mode = b.standardReleaseOptions();
    var target = b.standardTargetOptions(.{});
    // needed by sqlite
    target.setGnuLibCVersion(2, 28, 0);

    const run = addBin(
        b,
        b.addExecutable("run", "./bin/run.zig"),
        "run",
        "Evaluate an imp file",
    );
    commonSetup(run.bin, mode, target);
    zt.link(run.bin);
    run.run.addArgs(b.args orelse &.{});

    const test_unit = addBin(
        b,
        b.addTestExe("test_unit", "./bin/run.zig"),
        "test_unit",
        "Run unit tests",
    );
    commonSetup(test_unit.bin, mode, target);

    const test_step = b.step("test", "Run all tests");
    test_step.dependOn(test_unit.step);
}

fn addBin(
    b: *std.build.Builder,
    bin: *std.build.LibExeObjStep,
    name: []const u8,
    description: []const u8,
) struct {
    bin: *std.build.LibExeObjStep,
    run: *std.build.RunStep,
    step: *std.build.Step,
} {
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
