const Result = lib.Result(i64, u64);
pub fn solve(_: std.mem.Allocator, file_content: []const u8) !Result {
    var res = Result{ .first = 0, .second = 0 };

    for (file_content, 1..) |byte, idx| {
        switch (byte) {
            '(' => {
                res.first += 1;
            },
            ')' => {
                res.first -= 1;
                if (res.first < 0 and res.second == 0) {
                    res.second = idx;
                }
            },
            else => @panic("unreachable"),
        }
    }

    return res;
}

const std = @import("std");
const lib = @import("../extra/lib.zig");
