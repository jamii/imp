const std = @import("std");
const imp = @import("../imp.zig");
const u = imp.util;

const ig = @import("imgui");
const zt = @import("zt");
const zg = zt.custom_components;

pub fn run(storage: *imp.Storage) !void {
    const Context = zt.App(void);
    // TODO this can't be called twice
    var context = Context.begin(storage.arena.allocator()) catch unreachable;
    context.settings.energySaving = false;
    while (context.open) {
        context.beginFrame();

        // turn off internal windowing
        const viewport = ig.igGetMainViewport();
        ig.igSetNextWindowPos(viewport.*.Pos, 0, .{});
        ig.igSetNextWindowSize(viewport.*.Size, 0);

        var open = true;
        if (ig.igBegin(
            "The window",
            &open,
            ig.ImGuiWindowFlags_NoDecoration |
                ig.ImGuiWindowFlags_NoBackground |
                ig.ImGuiWindowFlags_AlwaysAutoResize |
                ig.ImGuiWindowFlags_NoSavedSettings |
                ig.ImGuiWindowFlags_NoFocusOnAppearing |
                ig.ImGuiWindowFlags_NoNav,
        )) {
            zg.text("hello world", .{});
        }
        ig.igEnd();

        context.endFrame();
    }
}
