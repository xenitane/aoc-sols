const Processor = struct {
    const NodeData = struct { char: u8, frq: u8 = 1 };
    const List = std.DoublyLinkedList(NodeData);

    str: []const u8,
    list: ?List = null,

    fn len(self: Processor) usize {
        if (self.list) |list| {
            var res: usize = 0;
            var node_op = list.first;
            while (node_op) |node| {
                res += 1 + digits(node.data.frq);
                node_op = node.next;
            }
            return res;
        }
        return self.str.len;
    }

    fn next(self: *Processor, ac: std.mem.Allocator) !void {
        if (self.list) |*old_list| {
            var list = List{};
            errdefer listDeinit(ac, &list);
            var buffer: [10]u8 = undefined;

            while (old_list.popFirst()) |node| {
                try addToListFromString(ac, try std.fmt.bufPrint(&buffer, "{d}{d}", .{ node.data.frq, node.data.char }), &list);
                ac.destroy(node);
            }

            self.list = list;
        } else {
            var list = List{};
            errdefer listDeinit(ac, &list);
            try addToListFromString(ac, self.str, &list);
            self.list = list;
        }
    }

    fn reset(self: *Processor, ac: std.mem.Allocator) void {
        if (self.list) |*list| {
            listDeinit(ac, list);
        }
        self.list = null;
    }

    fn deinit(self: *Processor, ac: std.mem.Allocator) void {
        self.reset(ac);
        self.* = undefined;
    }

    fn addToListFromString(ac: std.mem.Allocator, str: []const u8, list: *List) !void {
        for (str) |char| {
            if (list.last == null or list.last.?.data.char != char - 48) {
                const node = try ac.create(List.Node);
                node.data = .{ .char = char - 48 };
                list.append(node);
            } else {
                list.last.?.data.frq += 1;
            }
        }
    }

    fn listDeinit(ac: std.mem.Allocator, list: *std.DoublyLinkedList(NodeData)) void {
        while (list.pop()) |node| {
            ac.destroy(node);
        }
    }

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
};

const Result = lib.Result(usize, usize);
fn solve(ac: std.mem.Allocator, file_content: []const u8) !Result {
    var res = Result{ .first = undefined, .second = undefined };

    var processor = Processor{ .str = file_content };
    defer processor.deinit(ac);

    for (0..40) |_| {
        try processor.next(ac);
    }
    res.first = processor.len();

    for (40..50) |_| {
        try processor.next(ac);
    }
    res.second = processor.len();

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
