const HIGHEST_QUOT = 50;

const Result = lib.Result(usize, usize);
pub fn solve(_: std.mem.Allocator, file_content: []const u8) !Result {
    var res = Result{ .first = 0, .second = 0 };

    const target = try std.fmt.parseInt(usize, file_content, 10);

    for (1..@divFloor(target + 9, 10) + 1) |idx| {
        if ((sumFactors(idx) * 10) >= target) {
            res.first = idx;
            break;
        }
    }
    for (1..@divFloor(target + 10, 11) + 1) |idx| {
        if ((sumFactorsWithQuotientBelow(idx, HIGHEST_QUOT) * 11) >= target) {
            res.second = idx;
            break;
        }
    }

    return res;
}

inline fn sumFactorsWithQuotientBelow(num: usize, highest_quot: usize) usize {
    var res: usize = 0;
    for (1..highest_quot + 1) |i| {
        if (num % i == 0) {
            res += @divFloor(num, i);
        }
    }
    return res;
}

inline fn sumFactors(num: usize) usize {
    var base = num;
    var res: usize = 1;
    {
        var k: usize = 2;
        while (base & 1 == 0) {
            k <<= 1;
            base >>= 1;
        }
        res = k - 1;
    }

    var fac: usize = 3;
    while (base >= fac * fac) : (fac += 2) {
        var k: usize = fac;
        while (base % fac == 0) {
            k *= fac;
            base = @divFloor(base, fac);
        }
        res *= @divFloor(k - 1, fac - 1);
    }
    if (base > 1) {
        res *= base + 1;
    }
    return res;
}

const std = @import("std");
const lib = @import("../extra/lib.zig");
