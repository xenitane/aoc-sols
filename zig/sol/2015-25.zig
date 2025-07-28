const Result = lib.Result(usize, void);
fn solve(_: std.mem.Allocator, file_content: []const u8) !Result {
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
