const Result = lib.Result(usize, void);
pub fn solve(_: std.mem.Allocator, file_content: []const u8) !Result {
    var res = Result{ .first = 0, .second = undefined };

    var idx: usize = 0;
    while (idx < file_content.len and !std.ascii.isDigit(file_content[idx])) : (idx += 1) {}
    var row: usize = 0;
    while (idx < file_content.len and std.ascii.isDigit(file_content[idx])) : (idx += 1) {
        row = row * 10 + file_content[idx] - 48;
    }

    while (idx < file_content.len and !std.ascii.isDigit(file_content[idx])) : (idx += 1) {}
    var col: usize = 0;
    while (idx < file_content.len and std.ascii.isDigit(file_content[idx])) : (idx += 1) {
        col = col * 10 + file_content[idx] - 48;
    }

    res.first = valueAtIndex((((row + col - 1) * (row + col - 2)) / 2) + col);

    return res;
}

inline fn valueAtIndex(num: usize) usize {
    var res: usize = 20151125;
    var base: usize = 252533;
    const mod = 33554393;
    var pow = num - 1;
    while (pow > 0) : (pow >>= 1) {
        if (pow & 1 == 1) {
            res = @mod(res * base, mod);
        }
        base = @mod(base * base, mod);
    }
    return res;
}

const std = @import("std");
const lib = @import("../extra/lib.zig");
