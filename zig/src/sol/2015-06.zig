const DIM = 1000;

const BITMASKS = blk: {
    @setEvalBranchQuota(1001);
    var res: [DIM + 1]std.meta.Int(.unsigned, DIM) = undefined;
    res[0] = 0;
    for (0..DIM) |i| {
        res[i + 1] = 1 + (res[i] << 1);
    }
    break :blk res;
};

const Result = lib.Result(usize, usize);
pub fn solve(_: std.mem.Allocator, file_content: []const u8) !Result {
    var res = Result{ .first = 0, .second = 0 };

    const hash = std.hash.Wyhash.hash;

    var state0 = [_]std.meta.Int(.unsigned, DIM){0} ** DIM;
    var state1: [DIM][DIM]usize = undefined;
    for (0..DIM) |i| {
        for (0..DIM) |j| {
            state1[i][j] = 0;
        }
    }

    var lines = std.mem.tokenizeScalar(u8, file_content, '\n');
    while (lines.next()) |line| {
        const mode = blk: {
            const start: usize = 0;
            var end: usize = 0;
            while (!std.ascii.isDigit(line[end])) : (end += 1) {}
            break :blk line[start .. end - 1];
        };
        var rest = std.mem.tokenizeScalar(u8, line[mode.len + 1 ..], ' ');
        const start_x, const start_y = try getDims(&rest);
        _ = rest.next();
        const end_x, const end_y = try getDims(&rest);
        const mask = BITMASKS[end_y + 1] ^ BITMASKS[start_y];
        switch (hash(0, mode)) {
            hash(0, "turn on") => {
                for (start_x..end_x + 1) |idx| {
                    state0[idx] |= mask;
                    for (start_y..end_y + 1) |jdx| {
                        state1[idx][jdx] += 1;
                    }
                }
            },
            hash(0, "turn off") => {
                for (start_x..end_x + 1) |idx| {
                    state0[idx] &= ~mask;
                    for (start_y..end_y + 1) |jdx| {
                        state1[idx][jdx] -|= 1;
                    }
                }
            },
            hash(0, "toggle") => {
                for (start_x..end_x + 1) |idx| {
                    state0[idx] ^= mask;
                    for (start_y..end_y + 1) |jdx| {
                        state1[idx][jdx] += 2;
                    }
                }
            },
            else => @panic("unreachable"),
        }
    }

    for (0..DIM) |i| {
        res.first += @popCount(state0[i]);
        res.second += row_sum(&state1[i]);
    }
    return res;
}

inline fn row_sum(arr: []const usize) usize {
    var res: usize = 0;
    for (arr) |v| {
        res += v;
    }
    return res;
}
fn getDims(rest: *std.mem.TokenIterator(u8, std.mem.DelimiterType.scalar)) !struct { u10, u10 } {
    var dim_start = std.mem.tokenizeScalar(u8, rest.next().?, ',');
    return .{ try std.fmt.parseInt(u10, dim_start.next().?, 10), try std.fmt.parseInt(u10, dim_start.next().?, 10) };
}

const std = @import("std");
const lib = @import("../extra/lib.zig");
