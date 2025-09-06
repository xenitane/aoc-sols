const RACE_DURATION = 2503;

const Result = lib.Result(usize, usize);
pub fn solve(ac: std.mem.Allocator, file_content: []const u8) !Result {
    var res = Result{ .first = 0, .second = 0 };

    var deerInfoList = std.ArrayListUnmanaged([3]usize).empty;
    defer deerInfoList.deinit(ac);

    var lines = std.mem.tokenizeScalar(u8, file_content, '\n');
    while (lines.next()) |line| {
        var toks = std.mem.tokenizeScalar(u8, line, ' ');
        _, _, _, const speed, _, _, const flight_time, _, _, _, _, _, _, const rest_time, _ = .{
            toks.next(),
            toks.next(),
            toks.next(),
            try std.fmt.parseInt(usize, toks.next().?, 10),
            toks.next(),
            toks.next(),
            try std.fmt.parseInt(usize, toks.next().?, 10),
            toks.next(),
            toks.next(),
            toks.next(),
            toks.next(),
            toks.next(),
            toks.next(),
            try std.fmt.parseInt(usize, toks.next().?, 10),
            toks.next(),
        };

        try deerInfoList.append(ac, .{ speed, flight_time, rest_time });
    }

    for (deerInfoList.items) |deerInfo| {
        const dist = (@divFloor(RACE_DURATION, deerInfo[1] + deerInfo[2]) * deerInfo[1] + @min(deerInfo[1], @rem(RACE_DURATION, deerInfo[1] + deerInfo[2]))) * deerInfo[0];
        res.first = @max(res.first, dist);
    }

    var deerPosList = try std.ArrayListUnmanaged(struct { usize, usize, union(enum) { flight: usize, rest: usize } }).initCapacity(ac, deerInfoList.items.len);
    defer deerPosList.deinit(ac);

    var leaders = try std.ArrayListUnmanaged(usize).initCapacity(ac, deerInfoList.items.len);
    defer leaders.deinit(ac);

    for (deerInfoList.items) |info| {
        deerPosList.appendAssumeCapacity(.{ 0, 0, .{ .flight = info[1] } });
    }

    for (0..RACE_DURATION) |_| {
        leaders.clearRetainingCapacity();
        var max_dist: usize = 0;
        for (deerPosList.items, deerInfoList.items, 0..) |*pos, info, i| {
            switch (pos[2]) {
                .flight => |*val| {
                    pos[1] += info[0];
                    val.* -= 1;
                    if (val.* == 0) {
                        pos[2] = .{ .rest = info[2] };
                    }
                },
                .rest => |*val| {
                    val.* -= 1;
                    if (val.* == 0) {
                        pos[2] = .{ .flight = info[1] };
                    }
                },
            }
            if (max_dist < pos[1]) {
                max_dist = pos[1];
                leaders.clearRetainingCapacity();
            }
            if (max_dist == pos[1]) {
                leaders.appendAssumeCapacity(i);
            }
        }
        for (leaders.items) |i| {
            deerPosList.items[i][0] += 1;
        }
    }
    for (deerPosList.items) |pos| {
        res.second = @max(res.second, pos[0]);
    }

    return res;
}

const std = @import("std");
const lib = @import("../extra/lib.zig");
