const ITERATIONS = 100;
const DIM = 100;
const GRID = [DIM + 2][DIM + 2]u8;

const Result = lib.Result(usize, usize);
fn solve(ac: std.mem.Allocator, file_content: []const u8) !Result {
    var res = Result{ .first = 0, .second = 0 };

    const grids = try ac.alloc(GRID, 2);
    defer ac.free(grids);

    var grid_back: GRID = undefined;

    for (0..DIM + 2) |x| {
        for (0..DIM + 2) |y| {
            grids[0][x][y] = 0;
            grids[1][x][y] = 0;
            grid_back[x][y] = 0;
        }
    }

    var lines = std.mem.tokenizeScalar(u8, file_content, '\n');
    {
        var i: usize = 0;
        while (lines.next()) |line| : (i += 1) {
            if (line.len != DIM) {
                @panic("invalid input");
            }
            for (line, 0..) |char, j| {
                const x: u8 = switch (char) {
                    '.' => 0,
                    '#' => 1,
                    else => @panic("unreachable"),
                };
                grids[0][i + 1][j + 1] = x;
                grids[1][i + 1][j + 1] = x;
            }
        }
        if (i != DIM) {
            @panic("invalid input");
        }
    }

    for (0..ITERATIONS) |_| {
        for (0..2) |k| {
            nextIteration(&grids[k], &grid_back, k);
            std.mem.swap(GRID, &grids[k], &grid_back);
        }
    }

    for (0..DIM) |i| {
        for (0..DIM) |j| {
            res.first += grids[0][i + 1][j + 1];
            res.second += grids[1][i + 1][j + 1];
        }
    }

    return res;
}

inline fn nextIteration(grid_f: *const GRID, grid_b: *GRID, corners: usize) void {
    for (0..DIM) |x| {
        for (0..DIM) |y| {
            const nbors = countNeighbors(grid_f, x + 1, y + 1);
            grid_b[x + 1][y + 1] = switch ((nbors << 1) | grid_f[x + 1][y + 1]) {
                5, 6, 7 => 1,
                else => 0,
            };
        }
    }

    if (corners == 1) {
        inline for (.{ 1, 1, DIM, DIM }, .{ 1, DIM, 1, DIM }) |x, y| {
            grid_b[x][y] = 1;
        }
    }
}

inline fn countNeighbors(grid: *const GRID, i: usize, j: usize) u8 {
    if (i == 0 or j == 0 or i > DIM or j > DIM) {
        @panic("unreachable");
    }
    var res: u8 = 0;
    for (i - 1..i + 2) |x| {
        for (j - 1..j + 2) |y| {
            res += grid[x][y];
        }
    }

    return res - grid[i][j];
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
