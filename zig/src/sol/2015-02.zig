const Result = lib.Result(u64, u64);
pub fn solve(_: std.mem.Allocator, file_content: []const u8) !Result {
    var res = Result{ .first = 0, .second = 0 };

    var lines = std.mem.tokenizeScalar(u8, file_content, '\n');
    while (lines.next()) |line| {
        var dims_tok = std.mem.tokenizeScalar(u8, line, 'x');

        var dims: [3]u32 = undefined;

        for (0..3) |i| {
            dims[i] = try std.fmt.parseInt(u32, dims_tok.next().?, 10);
        }

        std.mem.sort(u32, &dims, {}, std.sort.asc(u32));

        res.first += 2 * (dims[0] * dims[1] + dims[1] * dims[2] + dims[2] * dims[0]) + dims[0] * dims[1];
        res.second += 2 * (dims[0] + dims[1]) + dims[0] * dims[1] * dims[2];
    }
    return res;
}

const std = @import("std");
const lib = @import("../extra/lib.zig");
