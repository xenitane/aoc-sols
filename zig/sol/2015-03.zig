const Result = lib.Result(u64, u64);
fn solve(ac: std.mem.Allocator, file_content: []const u8) !Result {
    const V2 = @Vector(2, i32);
    const PosSet = std.AutoArrayHashMapUnmanaged(V2, void);

    var res = Result{ .first = 0, .second = 0 };

    var visited = [_]PosSet{ PosSet.empty, PosSet.empty };

    for (0..visited.len) |i| {
        try visited[i].put(ac, .{ 0, 0 }, {});
    }

    defer {
        for (0..visited.len) |i| {
            visited[i].deinit(ac);
        }
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
