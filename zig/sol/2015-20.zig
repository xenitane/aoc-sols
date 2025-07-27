const Result = lib.Result(usize, usize);
fn solve(_: std.mem.Allocator, file_content: []const u8) !Result {
    var res = Result{ .first = 0, .second = 0 };

    const target = try std.fmt.parseInt(usize, file_content, 10);

    for (1..@divFloor(target + 9, 10) + 1) |idx| {
        if ((sumFactors(idx) * 10) >= target) {
            res.first = idx;
            break;
        }
    }
    for (1..@divFloor(target + 10, 11) + 1) |idx| {
        if ((sumFactorsWithQuotientAbove49(idx) * 11) >= target) {
            res.second = idx;
            break;
        }
    }

    return res;
}

inline fn sumFactorsWithQuotientAbove49(num: usize) usize {
    var res: usize = 0;
    for (1..51) |i| {
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
