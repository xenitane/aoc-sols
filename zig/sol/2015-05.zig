const Result = lib.Result(u64, u64);
fn solve(ac: std.mem.Allocator, file_content: []const u8) !Result {
    var res = Result{ .first = 0, .second = 0 };

    var lines = std.mem.tokenizeScalar(u8, file_content, '\n');
    while (lines.next()) |line| {
        var win = std.mem.window(u8, line, 3, 1);

        var vc: usize = 0;

        vc += isVowel(line[0]);
        vc += isVowel(line[1]);

        var duo = line[0] == line[1];
        var unwanted_pair = switch (f2U8ToU16(line[0], line[1])) {
            f2U8ToU16('a', 'b'), f2U8ToU16('c', 'd'), f2U8ToU16('p', 'q'), f2U8ToU16('x', 'y') => true,
            else => false,
        };

        var aba = false;
        var two_pairs = false;
        var pair_set = std.AutoArrayHashMapUnmanaged(u16, bool).empty;
        try pair_set.put(ac, f2U8ToU16(line[0], line[1]), false);
        defer pair_set.deinit(ac);

        while (win.next()) |seg| {
            vc += isVowel(seg[2]);

            if (seg[1] == seg[2]) {
                duo = true;
            }

            switch (f2U8ToU16(seg[1], seg[2])) {
                f2U8ToU16('a', 'b'), f2U8ToU16('c', 'd'), f2U8ToU16('p', 'q'), f2U8ToU16('x', 'y') => unwanted_pair = true,
                else => {},
            }

            if (seg[0] == seg[2]) {
                aba = true;
            }

            if (!two_pairs) {
                const pk = f2U8ToU16(seg[0], seg[1]);
                const key = f2U8ToU16(seg[1], seg[2]);

                const gop = try pair_set.getOrPut(ac, key);

                if (!gop.found_existing) {
                    gop.value_ptr.* = false;
                    continue;
                }
                if (pk == key and !gop.value_ptr.*) {
                    gop.value_ptr.* = true;
                    continue;
                }
                pair_set.clearAndFree(ac);
                two_pairs = true;
            }
        }

        if (!unwanted_pair and duo and vc > 2) {
            res.first += 1;
        }
        if (aba and two_pairs) {
            res.second += 1;
        }
    }

    return res;
}

fn f2U8ToU16(a: u8, b: u8) u16 {
    var res: u16 = undefined;
    const res_bytes = std.mem.asBytes(&res);
    res_bytes[0], res_bytes[1] = .{ a, b };
    return res;
}

fn isVowel(c: u8) u1 {
    return switch (c) {
        'a', 'e', 'i', 'o', 'u' => 1,
        else => 0,
    };
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
