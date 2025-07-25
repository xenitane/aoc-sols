const Result = lib.Result(isize, isize);
fn solve(ac: std.mem.Allocator, file_content: []const u8) !Result {
    var res = Result{ .first = undefined, .second = undefined };

    var json = try std.json.parseFromSlice(std.json.Value, ac, file_content, .{});
    defer json.deinit();

    res.first, res.second = getSum(json.value);

    return res;
}

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
            var second = true;
            var itr = object.iterator();
            while (itr.next()) |kv| {
                switch (kv.value_ptr.*) {
                    .string => |string| {
                        if (std.mem.eql(u8, string, "red")) {
                            second = false;
                        }
                    },
                    else => {
                        temp += getSum(kv.value_ptr.*);
                    },
                }
            }
            if (!second) temp[1] = 0;
            res += temp;
        },
        else => {},
    }
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
