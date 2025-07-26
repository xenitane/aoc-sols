const StringPair = struct {
    fn lessThanFn(_: void, a: [2][]const u8, b: [2][]const u8) bool {
        return (a[1].len > b[0].len);
    }
};

const Result = lib.Result(usize, usize);
fn solve(ac: std.mem.Allocator, file_content: []const u8) !Result {
    var res = Result{ .first = 0, .second = 0 };

    var possible_molecules = std.StringArrayHashMapUnmanaged(void).empty;
    defer possible_molecules.deinit(ac);

    var transforms = std.ArrayListUnmanaged([2][]const u8).empty;
    defer transforms.deinit(ac);

    var lines = std.mem.splitScalar(u8, file_content, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) {
            break;
        }
        var toks = std.mem.tokenizeScalar(u8, line, ' ');
        const from, _, const to = .{ toks.next().?, toks.next(), toks.next().? };
        try transforms.append(ac, .{ from, to });
    }

    const given_molecule = lines.next().?;

    {
        const buffer = try ac.alloc(u8, given_molecule.len * 2);
        defer ac.free(buffer);

        for (transforms.items) |transform| {
            var k: usize = 0;
            while (k < given_molecule.len) {
                if (std.mem.indexOfPos(u8, given_molecule, k, transform[0])) |idx| {
                    const new_molecule = try std.fmt.bufPrint(buffer, "{s}{s}{s}", .{
                        given_molecule[0..idx],
                        transform[1],
                        given_molecule[idx + transform[0].len ..],
                    });
                    _ = try possible_molecules.getOrPut(ac, new_molecule);
                    k = idx + transform[0].len;
                } else {
                    k += 1;
                }
            }
        }
        res.first = possible_molecules.count();
    }

    {
        const buffer0 = try ac.alloc(u8, given_molecule.len * 2);
        defer ac.free(buffer0);
        const buffer1 = try ac.alloc(u8, given_molecule.len * 2);
        defer ac.free(buffer1);

        @memset(buffer0, 0);
        @memset(buffer1, 0);

        var cur_molecule = try std.fmt.bufPrint(buffer0, "{s}", .{given_molecule});

        var rng = std.Random.DefaultPrng.init(@bitCast(std.time.timestamp()));
        const rand = rng.random();

        var trials: usize = transforms.items.len * cur_molecule.len;

        while (!std.mem.eql(u8, cur_molecule, "e") and trials > 0) {
            const ii = rand.uintLessThan(usize, transforms.items.len);

            trials -= 1;
            if (std.mem.indexOf(u8, cur_molecule, transforms.items[ii][1])) |idx| {
                const refined_molecule = try std.fmt.bufPrint(buffer1, "{s}{s}{s}", .{
                    cur_molecule[0..idx],
                    transforms.items[ii][0],
                    cur_molecule[idx + transforms.items[ii][1].len ..],
                });
                cur_molecule = try std.fmt.bufPrint(buffer0, "{s}", .{refined_molecule});
                res.second += 1;
            }
        }
        if (!std.mem.eql(u8, "e", cur_molecule)) {
            @panic("run again");
        }
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
