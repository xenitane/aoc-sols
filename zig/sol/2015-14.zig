const Result = lib.Result(usize, usize);
const RACE_DURATION = 2503;
fn solve(ac: std.mem.Allocator, file_content: []const u8) !Result {
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
            switch (pos.@"2") {
                .flight => |*val| {
                    pos.@"1" += info[0];
                    val.* -= 1;
                    if (val.* == 0) {
                        pos.@"2" = .{ .rest = info[2] };
                    }
                },
                .rest => |*val| {
                    val.* -= 1;
                    if (val.* == 0) {
                        pos.@"2" = .{ .flight = info[1] };
                    }
                },
            }
            if (max_dist < pos.@"1") {
                max_dist = pos.@"1";
                leaders.clearRetainingCapacity();
            }
            if (max_dist == pos.@"1") {
                leaders.appendAssumeCapacity(i);
            }
        }
        for (leaders.items) |i| {
            deerPosList.items[i].@"0" += 1;
        }
    }
    for (deerPosList.items) |pos| {
        res.second = @max(res.second, pos.@"0");
    }

    return res;
}

const std = @import("std");
const lib = @import("lib/root.zig");
const opts = @import("opts");

pub fn main() !void {
    var da = std.heap.DebugAllocator(.{}).init;
    defer {
        if (da.deinit() == std.heap.Check.leak) {
            @panic("!!!memory leak detected!!!\n");
        }
    }

    const ac = da.allocator();

    const file_content = try lib.util.readEntireFile(ac, opts.input_file_path);
    defer ac.free(file_content);

    const res = try solve(ac, file_content);
    try res.print(std.io.getStdOut().writer());
}

test solve {
    const ally = std.testing.allocator;

    const in_file_content = try lib.util.readEntireFile(ally, opts.test_input_file_path);
    defer ally.free(in_file_content);
    const out_file_content = try lib.util.readEntireFile(ally, opts.test_output_file_path);
    defer ally.free(out_file_content);

    const res = try solve(ally, in_file_content);
    var string_builder = std.ArrayList(u8).init(ally);
    defer string_builder.deinit();
    try res.print(string_builder.writer());

    while (string_builder.getLastOrNull()) |elem| {
        if (!std.ascii.isWhitespace(elem)) {
            break;
        }
        _ = string_builder.pop();
    }

    try std.testing.expectEqualStrings(out_file_content, string_builder.items);
}
