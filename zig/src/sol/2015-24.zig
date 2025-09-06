const SELECTED = @as(usize, 1) << (@bitSizeOf(usize) - 1);

const Result = lib.Result(usize, usize);
pub fn solve(ac: std.mem.Allocator, file_content: []const u8) !Result {
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
const lib = @import("../extra/lib.zig");
