const std = @import("std");
const imp = @import("../imp.zig");
const u = imp.util;

const ig = @import("imgui");
const zt = @import("zt");
const zg = zt.custom_components;

pub fn run(storage: *imp.Storage) !void {
    const inserts = try storage.getLiveInserts();
    var rules = try storage.arena.allocator().alloc([]u8, inserts.len);
    for (rules) |*rule, i|
        rule.* = try storage.arena.allocator().dupeZ(u8, inserts[i].rule);

    const Context = zt.App(void);
    // TODO this can't be called twice
    var context = Context.begin(storage.arena.allocator()) catch unreachable;
    context.settings.energySaving = false;
    const style = ig.igGetStyle();
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
            for (rules) |*rule, i| {
                var num_newlines: usize = 0;
                for (std.mem.sliceTo(rule.*, 0)) |char| {
                    if (char == '\n') {
                        num_newlines += 1;
                    }
                }
                const UserData = struct {
                    rule: *[]u8,
                    storage: *imp.Storage,
                };
                var user_data = UserData{
                    .rule = rule,
                    .storage = storage,
                };
                _ = ig.igInputTextMultiline(
                    zg.fmtTextForImgui("## {} {}", .{ inserts[i].tx_id, inserts[i].rule_id }),
                    rule.ptr,
                    rule.len + 1,
                    .{
                        .x = 0,
                        .y = @intToFloat(f32, num_newlines + 1) * ig.igGetTextLineHeight() + 2 * style.*.FramePadding.y + 1,
                    },
                    ig.ImGuiInputTextFlags_CallbackResize,
                    struct {
                        export fn resize(data: [*c]ig.ImGuiInputTextCallbackData) c_int {
                            if (data.*.EventFlag == ig.ImGuiInputTextFlags_CallbackResize) {
                                const my_user_data = @ptrCast(*UserData, @alignCast(@alignOf(UserData), data.*.UserData));
                                my_user_data.rule.* = my_user_data.storage.arena.allocator().realloc(my_user_data.rule.*, @intCast(usize, data.*.BufSize + 1)) catch unreachable;
                                data.*.Buf = my_user_data.rule.ptr;
                            }
                            return 0;
                        }
                    }.resize,
                    @ptrCast(*anyopaque, &user_data),
                );
            }
        }
        ig.igEnd();

        context.endFrame();
    }
}
