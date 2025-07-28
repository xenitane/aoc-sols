const Perm = struct {
    seq: []usize,

    fn init(ac: std.mem.Allocator, n: usize) !Perm {
        const res = Perm{ .seq = try ac.alloc(usize, n) };
        for (res.seq, 1..) |*p, i| {
            p.* = i;
        }
        return res;
    }

    fn deinit(self: *Perm, ac: std.mem.Allocator) void {
        ac.free(self.seq);
        self.* = undefined;
    }

    fn next(self: *Perm) bool {
        var end = self.seq.len - 1;
        while (end > 0) : (end -= 1) {
            if (self.seq[end - 1] < self.seq[end]) {
                std.sort.block(usize, self.seq[end..], {}, std.sort.asc(usize));
                const sp = blk: {
                    var l = end;
                    var r = self.seq.len - 1;
                    while (l < r) {
                        const m = @divFloor(r - l, 2) + l;
                        if (self.seq[m] <= self.seq[end - 1]) {
                            l = m + 1;
                        } else {
                            r = m;
                        }
                    }
                    break :blk l;
                };
                std.mem.swap(usize, &self.seq[end - 1], &self.seq[sp]);
                return true;
            }
        }
        return false;
    }

    fn calcDist(self: Perm, dists: DistMap) usize {
        var res: usize = 0;
        for (1..self.seq.len) |i| {
            res += dists.get(.{ @min(self.seq[i - 1], self.seq[i]), @max(self.seq[i - 1], self.seq[i]) }).?;
        }
        return res;
    }
};

const DistMap = std.AutoArrayHashMapUnmanaged([2]usize, usize);

const Result = lib.Result(usize, usize);
fn solve(ac: std.mem.Allocator, file_content: []const u8) !Result {
    var res = Result{ .first = undefined, .second = undefined };

    var city_name_ids = std.StringArrayHashMapUnmanaged(usize).empty;

    var dists = DistMap.empty;
    defer dists.deinit(ac);

    var lines = std.mem.tokenizeScalar(u8, file_content, '\n');
    while (lines.next()) |line| {
        var toks = std.mem.tokenizeScalar(u8, line, ' ');
        const city1, _, const city2, _, const dist = .{
            try getOrPutCityId(&city_name_ids, ac, toks.next().?),
            toks.next(),
            try getOrPutCityId(&city_name_ids, ac, toks.next().?),
            toks.next(),
            try std.fmt.parseInt(usize, toks.next().?, 10),
        };

        try dists.put(ac, .{ @min(city1, city2), @max(city1, city2) }, dist);
    }
    const cities = city_name_ids.count();
    city_name_ids.deinit(ac);

    var perm = try Perm.init(ac, cities);
    defer perm.deinit(ac);

    res.first = perm.calcDist(dists);
    res.second = res.first;

    while (perm.next()) {
        const new_dist = perm.calcDist(dists);
        res.first = @min(res.first, new_dist);
        res.second = @max(res.second, new_dist);
    }

    return res;
}

fn getOrPutCityId(city_name_ids: *std.StringArrayHashMapUnmanaged(usize), ac: std.mem.Allocator, city: []const u8) !usize {
    const gop = try city_name_ids.getOrPut(ac, city);
    if (!gop.found_existing) {
        gop.value_ptr.* = city_name_ids.count();
    }
    return gop.value_ptr.*;
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
