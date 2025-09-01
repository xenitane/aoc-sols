const Processor = struct {
    const List = std.ArrayListUnmanaged([2]u8);

    str: []const u8,
    list0: ?List = null,
    list1: List = List.empty,

    fn len(self: Processor) usize {
        if (self.list0) |list| {
            var res: usize = 0;
            for (list.items) |item| {
                res += 1 + digits(item[1]);
            }
            return res;
        }
        return self.str.len;
    }

    fn next(self: *Processor, ac: std.mem.Allocator) !void {
        if (self.list0) |*old_list| {
            self.list1.clearRetainingCapacity();
            errdefer self.list1.deinit(ac);
            var buffer: [10]u8 = undefined;

            for (old_list.items) |item| {
                try addToListFromString(ac, try std.fmt.bufPrint(&buffer, "{d}{d}", .{ item[1], item[0] }), &self.list1);
            }
            std.mem.swap(List, &self.list1, old_list);
        } else {
            self.list0 = List{};
            errdefer self.list0.?.deinit(ac);
            try addToListFromString(ac, self.str, &self.list0.?);
        }
    }

    fn deinit(self: *Processor, ac: std.mem.Allocator) void {
        if (self.list0) |*list| {
            list.deinit(ac);
        }
        self.list1.deinit(ac);
        self.* = undefined;
    }

    fn addToListFromString(ac: std.mem.Allocator, str: []const u8, list: *List) !void {
        for (str) |char| {
            if (list.items.len == 0 or list.getLast()[0] != char - 48) {
                try list.append(ac, .{ char - 48, 0 });
            }
            list.items[list.items.len - 1][1] += 1;
        }
    }
};

inline fn digits(num: u8) u8 {
    if (num == 0) {
        @panic("0 not allowed");
    }
    inline for (.{ 100, 10, 1 }, .{ 3, 2, 1 }) |base, lg| {
        if (num >= base) {
            return lg;
        }
    }
    @panic("unreachable");
}

const ITER = .{ .{ 0, 40 }, .{ 40, 50 } };

const Result = lib.Result(usize, usize);
pub fn solve(ac: std.mem.Allocator, file_content: []const u8) !Result {
    var res = Result{ .first = undefined, .second = undefined };

    var processor = Processor{ .str = file_content };
    defer processor.deinit(ac);

    for (ITER[0][0]..ITER[0][1]) |_| {
        try processor.next(ac);
    }
    res.first = processor.len();

    for (ITER[1][0]..ITER[1][1]) |_| {
        try processor.next(ac);
    }
    res.second = processor.len();

    return res;
}

const std = @import("std");
const lib = @import("../extra/lib.zig");
