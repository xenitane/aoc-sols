const SELECTED = @as(usize, 1) << (@bitSizeOf(usize) - 1);

const Result = lib.Result(usize, usize);
fn solve(ac: std.mem.Allocator, file_content: []const u8) !Result {
    var res = Result{ .first = std.math.maxInt(usize), .second = std.math.maxInt(usize) };

    var packages = std.ArrayListUnmanaged(usize).empty;
    defer packages.deinit(ac);

    var lines = std.mem.tokenizeScalar(u8, file_content, '\n');
    var total_weight: usize = 0;
    while (lines.next()) |line| {
        try packages.append(ac, try std.fmt.parseInt(usize, line, 10));
        total_weight += packages.getLast();
    }

    if (@mod(total_weight, 12) != 0) {
        @panic("cannot divide packages");
    }

    var min_packages_used = packages.items.len;

    findMinEntanglement(packages.items, 0, 0, 1, 0, total_weight / 3, &res.first, &min_packages_used);

    min_packages_used = packages.items.len;
    findMinEntanglement(packages.items, 0, 0, 1, 0, total_weight / 4, &res.second, &min_packages_used);

    return res;
}

fn findMinEntanglement(packages: []usize, idx: usize, weight: usize, prod: usize, packages_used: usize, target: usize, res: *usize, mp: *usize) void {
    if (weight > target) {
        return;
    }
    if (weight == target) {
        if (packages_used > mp.*) {
            return;
        }
        if (!seperatable(packages, 0, 0, target)) {
            return;
        }
        if (packages_used < mp.*) {
            mp.* = packages_used;
            res.* = prod;
        }
        if (packages_used == mp.*) {
            res.* = @min(res.*, prod);
        }
    }
    if (idx == packages.len) {
        return;
    }

    findMinEntanglement(packages, idx + 1, weight, prod, packages_used, target, res, mp);

    const package = packages[idx];
    packages[idx] += SELECTED;
    findMinEntanglement(packages, idx + 1, weight + package, prod * package, packages_used + 1, target, res, mp);
    packages[idx] = package;
}

fn seperatable(packages: []usize, idx: usize, weight: usize, target: usize) bool {
    if (weight > target) {
        return false;
    }
    if (weight == target) {
        return seperatable(packages, 0, 0, target);
    }
    if (idx == packages.len) {
        return allSelected(packages);
    }
    if (seperatable(packages, idx + 1, weight, target)) {
        return true;
    }

    if (packages[idx] < SELECTED) {
        const package = packages[idx];
        packages[idx] += SELECTED;
        const sepable = seperatable(packages, idx + 1, weight + package, target);
        packages[idx] = package;
        return sepable;
    }

    return false;
}

inline fn allSelected(packages: []const usize) bool {
    for (packages) |p| {
        if (p < SELECTED) {
            return false;
        }
    }
    return true;
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
