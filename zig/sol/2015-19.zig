const TARGET_MOLECULE = "e";

const Result = lib.Result(usize, usize);
pub fn solve(ac: std.mem.Allocator, file_content: []const u8) !Result {
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

    while (true) {
        res.second = 0;
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

        while (!std.mem.eql(u8, cur_molecule, TARGET_MOLECULE) and trials > 0) {
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
        if (std.mem.eql(u8, "e", cur_molecule)) {
            break;
        }
    }

    return res;
}

const std = @import("std");
const lib = @import("../extra/lib.zig");
