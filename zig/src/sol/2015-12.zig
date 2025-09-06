const Result = lib.Result(isize, isize);
pub fn solve(ac: std.mem.Allocator, file_content: []const u8) !Result {
    var res = Result{ .first = undefined, .second = undefined };

    var json = try std.json.parseFromSlice(std.json.Value, ac, file_content, .{});
    defer json.deinit();

    res.first, res.second = getSum(json.value);

    return res;
}

const DISQUALIFIER = "red";

const V2 = @Vector(2, isize);
fn getSum(jsonVal: std.json.Value) V2 {
    var res = V2{ 0, 0 };
    switch (jsonVal) {
        .integer => |int| {
            res += V2{ int, int };
        },
        .array => |array| {
            for (array.items) |item| {
                res += getSum(item);
            }
        },
        .object => |object| {
            var temp = V2{ 0, 0 };
            var not_second = false;
            var itr = object.iterator();
            while (itr.next()) |kv| {
                switch (kv.value_ptr.*) {
                    .string => |string| {
                        if (std.mem.eql(u8, string, DISQUALIFIER)) {
                            not_second = true;
                        }
                    },
                    else => {
                        temp += getSum(kv.value_ptr.*);
                    },
                }
            }
            if (not_second) temp[1] = 0;
            res += temp;
        },
        else => {},
    }
    return res;
}

const std = @import("std");
const lib = @import("../extra/lib.zig");
