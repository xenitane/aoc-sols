const Result = lib.Result(usize, usize);
pub fn solve(_: std.mem.Allocator, file_content: []const u8) !Result {
    var res = Result{ .first = 0, .second = 0 };

    var lines = std.mem.tokenizeScalar(u8, file_content, '\n');
    while (lines.next()) |line| {
        res.first += 2;
        res.second += 4;
        const data = line[1 .. line.len - 1];
        var idx: usize = 0;
        while (idx < data.len) : (idx += 1) {
            if (data[idx] != '\\') {
                continue;
            }
            idx += 1;
            res.first += 1;
            res.second += 1;
            if (data[idx] != 'x') {
                res.second += 1;
                continue;
            }
            idx += 2;
            res.first += 2;
        }
    }

    return res;
}

const std = @import("std");
const lib = @import("../extra/lib.zig");
