const HappinessMap = std.AutoArrayHashMapUnmanaged([2]usize, isize);
const PeopleNameIdMap = std.StringArrayHashMapUnmanaged(usize);

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

    fn calcHappiness(self: Perm, happiness: HappinessMap) isize {
        var res: isize = 0;
        for (0..self.seq.len) |i| {
            res += happiness.get(.{ self.seq[i], self.seq[(i + 1) % self.seq.len] }).?;
            res += happiness.get(.{ self.seq[(i + 1) % self.seq.len], self.seq[i] }).?;
        }
        return res;
    }
};

const Result = lib.Result(isize, isize);
fn solve(ac: std.mem.Allocator, file_content: []const u8) !Result {
    var res = Result{ .first = undefined, .second = undefined };

    var happiness = HappinessMap.empty;
    defer happiness.deinit(ac);

    var people_name_ids = PeopleNameIdMap.empty;

    var lines = std.mem.tokenizeScalar(u8, file_content, '\n');
    while (lines.next()) |line| {
        var toks = std.mem.tokenizeAny(u8, line, ". ");
        const p1, _, const happ_diff, _, _, _, _, _, _, const p2 = .{
            try getOrPutPeopleId(&people_name_ids, ac, toks.next().?),
            toks.next(),
            try getHappinessDiff(toks.next().?, toks.next().?),
            toks.next(),
            toks.next(),
            toks.next(),
            toks.next(),
            toks.next(),
            toks.next(),
            try getOrPutPeopleId(&people_name_ids, ac, toks.next().?),
        };

        try happiness.put(ac, .{ p1, p2 }, happ_diff);
    }

    const people = people_name_ids.count();
    for (1..people + 1) |i| {
        try happiness.put(ac, .{ people + 1, i }, 0);
        try happiness.put(ac, .{ i, people + 1 }, 0);
    }

    people_name_ids.deinit(ac);

    var perm: Perm = undefined;

    inline for (@typeInfo(Result).@"struct".fields, 0..) |field, i| {
        perm = try Perm.init(ac, people + i);
        @field(res, field.name) = perm.calcHappiness(happiness);
        while (perm.next()) {
            @field(res, field.name) = @max(@field(res, field.name), perm.calcHappiness(happiness));
        }
        perm.deinit(ac);
    }

    return res;
}

fn getHappinessDiff(sign: []const u8, diff: []const u8) !isize {
    const hash = std.hash.Wyhash.hash;
    const val = try std.fmt.parseInt(isize, diff, 10);
    return switch (hash(0, sign)) {
        hash(0, "gain") => val,
        hash(0, "lose") => -val,
        else => @panic("unreachable"),
    };
}

fn getOrPutPeopleId(people_name_ids: *std.StringArrayHashMapUnmanaged(usize), ac: std.mem.Allocator, person: []const u8) !usize {
    const gop = try people_name_ids.getOrPut(ac, person);
    if (!gop.found_existing) {
        gop.value_ptr.* = people_name_ids.count();
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
