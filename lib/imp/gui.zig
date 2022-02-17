const std = @import("std");
const imp = @import("../imp.zig");
const u = imp.util;

const ig = @import("imgui");
const zt = @import("zt");
const zg = zt.custom_components;

pub fn run(storage: *imp.Storage) !void {
    var inserts = u.ArrayList(imp.Storage.Insert).init(storage.arena.allocator());
    try inserts.appendSlice(try storage.getLiveInserts());
    var rules = u.ArrayList([]u8).init(storage.arena.allocator());
    for (inserts.items) |insert|
        try rules.append(try storage.arena.allocator().dupeZ(u8, insert.rule));

    var rng = try @import("../../bin/run.zig").newRng();
    var focus_rule_ix: ?usize = null;

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
            const Action = union(enum) {
                Insert: usize,
            };
            var action: ?Action = null;
            for (rules.items) |*rule, rule_ix| {
                var num_newlines: usize = 0;
                for (std.mem.sliceTo(rule.*, 0)) |char| {
                    if (char == '\n') {
                        num_newlines += 1;
                    }
                }
                const UserData = struct {
                    action: *?Action,
                    rule_ix: usize,
                    rule: *[]u8,
                    storage: *imp.Storage,
                };
                var user_data = UserData{
                    .action = &action,
                    .rule_ix = rule_ix,
                    .rule = rule,
                    .storage = storage,
                };
                if (focus_rule_ix == rule_ix) {
                    focus_rule_ix = null;
                    ig.igSetKeyboardFocusHere(0);
                }
                _ = ig.igInputTextMultiline(
                    zg.fmtTextForImgui("## {} {}", .{ inserts.items[rule_ix].tx_id, inserts.items[rule_ix].rule_id }),
                    rule.ptr,
                    rule.len + 1,
                    .{
                        .x = 0,
                        .y = @intToFloat(f32, num_newlines + 1) * ig.igGetTextLineHeight() + 2 * style.*.FramePadding.y + 1,
                    },
                    ig.ImGuiInputTextFlags_CallbackResize |
                        ig.ImGuiInputTextFlags_CallbackCharFilter,
                    struct {
                        export fn resize(data: [*c]ig.ImGuiInputTextCallbackData) c_int {
                            const my_user_data = @ptrCast(*UserData, @alignCast(@alignOf(UserData), data.*.UserData));
                            switch (data.*.EventFlag) {
                                ig.ImGuiInputTextFlags_CallbackResize => {
                                    my_user_data.rule.* = my_user_data.storage.arena.allocator().realloc(my_user_data.rule.*, @intCast(usize, data.*.BufSize + 1)) catch unreachable;
                                    data.*.Buf = my_user_data.rule.ptr;
                                    return 0;
                                },
                                ig.ImGuiInputTextFlags_CallbackCharFilter => {
                                    if (data.*.EventChar == '\n' and ig.igGetIO().*.KeyShift) {
                                        my_user_data.action.* = .{ .Insert = my_user_data.rule_ix };
                                        return 1;
                                    } else {
                                        return 0;
                                    }
                                },
                                else => unreachable,
                            }
                        }
                    }.resize,
                    @ptrCast(*anyopaque, &user_data),
                );
            }
            if (action) |a| {
                switch (a) {
                    .Insert => |rule_ix| {
                        try rules.insert(rule_ix + 1, "");
                        try inserts.insert(rule_ix + 1, .{
                            .tx_id = 0,
                            .rule_id = rng.random().int(imp.RuleId),
                            .rule = "",
                        });
                        focus_rule_ix = rule_ix + 1;
                    },
                }
            }
        }
        ig.igEnd();

        context.endFrame();
    }
}
