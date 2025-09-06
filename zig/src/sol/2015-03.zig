const Result = lib.Result(u64, u64);
pub fn solve(ac: std.mem.Allocator, file_content: []const u8) !Result {
    const V2 = @Vector(2, i32);
    const PosSet = std.AutoArrayHashMapUnmanaged(V2, void);

    var res = Result{ .first = 0, .second = 0 };

    var visited = [_]PosSet{ PosSet.empty, PosSet.empty };
    defer {
        for (0..visited.len) |i| {
            visited[i].deinit(ac);
        }
    }

    for (0..visited.len) |i| {
        try visited[i].put(ac, .{ 0, 0 }, {});
    }

    var positions = [_]V2{.{ 0, 0 }} ** 3;
    var x: u2 = 1;

    for (file_content) |byte| {
        const dv = switch (byte) {
            '^' => V2{ 1, 0 },
            'v' => V2{ -1, 0 },
            '<' => V2{ 0, -1 },
            '>' => V2{ 0, 1 },
            else => @panic("unreachable"),
        };
        positions[0] += dv;
        positions[x] += dv;

        try visited[0].put(ac, positions[0], {});
        try visited[1].put(ac, positions[x], {});

        x = 3 - x;
    }

    res.first = visited[0].count();
    res.second = visited[1].count();

    return res;
}

const std = @import("std");
const lib = @import("../extra/lib.zig");
