const Pass = struct {
    str: [8]u8,

    fn init(str: []const u8) Pass {
        if (str.len != 8) {
            @panic("unacceptable");
        }
        var res: Pass = undefined;
        @memcpy(&res.str, str);
        return res;
    }

    fn next(self: *Pass) void {
        while (true) {
            var idx = self.str.len;
            while (idx > 0) : (idx -= 1) {
                if (self.str[idx - 1] != 'z') {
                    self.str[idx - 1] += 1;
                    break;
                } else {
                    self.str[idx - 1] = 'a';
                }
            }
            if (validPass(self.str)) {
                return;
            }
        }
    }

    fn validPass(str: [8]u8) bool {
        for (0..2) |i| {
            switch (str[i]) {
                'i', 'l', 'o' => return false,
                else => {},
            }
        }
        var pair = [_]bool{ false, str[0] == str[1] };
        var two_pair = false;
        var trip = false;
        var win = std.mem.window(u8, &str, 3, 1);
        while (win.next()) |slide| {
            switch (slide[2]) {
                'i', 'l', 'o' => return false,
                else => {},
            }
            if (slide[0] + 1 == slide[1] and slide[1] + 1 == slide[2]) {
                trip = true;
            }
            if (!two_pair and slide[1] == slide[2] and pair[0]) {
                two_pair = true;
            } else {
                pair = .{ pair[0] or pair[1], slide[1] == slide[2] };
            }
        }
        return trip and two_pair;
    }
};

const Result = lib.Result([8]u8, [8]u8);
fn solve(_: std.mem.Allocator, file_content: []const u8) !Result {
    var res = Result{ .first = undefined, .second = undefined };

    var password = Pass.init(file_content);

    password.next();
    res.first = password.str;

    password.next();
    res.second = password.str;

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
