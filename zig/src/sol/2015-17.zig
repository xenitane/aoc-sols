const EGGNOG_QTY: usize = if (@import("builtin").is_test) 25 else 150;
const DPCell = struct { u32, []u32 };

const Result = lib.Result(u32, u32);
pub fn solve(ac: std.mem.Allocator, file_content: []const u8) !Result {
    var res = Result{ .first = 0, .second = 0 };

    var containers = std.ArrayListUnmanaged(u16).empty;
    defer containers.deinit(ac);

    var lines = std.mem.tokenizeScalar(u8, file_content, '\n');
    while (lines.next()) |line| {
        try containers.append(ac, try std.fmt.parseInt(u16, line, 10));
    }

    var dp = try ac.alloc([]?DPCell, containers.items.len + 1);
    defer ac.free(dp);

    for (0..containers.items.len + 1) |i| {
        errdefer for (dp[0..i]) |*prev_row| {
            ac.free(prev_row.*);
        };
        dp[i] = try ac.alloc(?DPCell, EGGNOG_QTY + 1);
        @memset(dp[i], null);
    }

    defer for (0..containers.items.len + 1) |i| {
        ac.free(dp[i]);
    };
    defer for (0..containers.items.len + 1) |i| {
        for (0..EGGNOG_QTY + 1) |j| {
            if (null == dp[i][j]) {
                continue;
            }
            ac.free(dp[i][j].?[1]);
        }
    };

    try buildDP(ac, containers.items, dp, 0, 0);

    res.first = dp[0][0].?[0];
    for (dp[0][0].?[1]) |val| {
        if (val > 0) {
            res.second = val;
            break;
        }
    }

    return res;
}

fn buildDP(ac: std.mem.Allocator, containers: []const u16, dp: []const []?DPCell, i: usize, j: usize) !void {
    if (j == EGGNOG_QTY) {
        dp[i][j] = .{ 1, try ac.alloc(u32, containers.len + 1) };
        @memset(dp[i][j].?[1], 0);
        dp[i][j].?[1][0] = 1;
        return;
    }

    if (null != dp[i][j]) {
        return;
    }

    dp[i][j] = .{ 0, try ac.alloc(u32, containers.len + 1) };
    @memset(dp[i][j].?[1], 0);

    if (i == containers.len) {
        return;
    }

    var ddp: DPCell = undefined;

    try buildDP(ac, containers, dp, i + 1, j);
    ddp = dp[i + 1][j].?;

    dp[i][j].?[0] += ddp[0];
    for (dp[i][j].?[1], ddp[1]) |*src, val| {
        src.* += val;
    }

    if (j + containers[i] > EGGNOG_QTY) {
        return;
    }

    try buildDP(ac, containers, dp, i + 1, j + containers[i]);
    ddp = dp[i + 1][j + containers[i]].?;

    dp[i][j].?[0] += ddp[0];
    for (dp[i][j].?[1][1..], ddp[1][0..containers.len]) |*src, val| {
        src.* += val;
    }
}

const std = @import("std");
const lib = @import("../extra/lib.zig");
