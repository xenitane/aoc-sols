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
};

inline fn validPass(str: [8]u8) bool {
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

const Result = lib.Result([8]u8, [8]u8);
pub fn solve(_: std.mem.Allocator, file_content: []const u8) !Result {
    var res = Result{ .first = undefined, .second = undefined };

    var password = Pass.init(file_content);

    password.next();
    res.first = password.str;

    password.next();
    res.second = password.str;

    return res;
}

const std = @import("std");
const lib = @import("../extra/lib.zig");
