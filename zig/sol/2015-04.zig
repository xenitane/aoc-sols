const Result = lib.Result(u64, u64);
fn solve(_: std.mem.Allocator, file_content: []const u8) !Result {
    const Md5 = std.crypto.hash.Md5;

    var res = Result{ .first = 0, .second = 0 };

    var buffer: [100]u8 = undefined;
    @memcpy(buffer[0..file_content.len], file_content);

    var hash_buffer: [Md5.digest_length]u8 = undefined;

    var idx: usize = 1;
    while (true) : (idx += 1) {
        const number_string = try std.fmt.bufPrint(buffer[file_content.len..], "{d}", .{idx});

        Md5.hash(buffer[0 .. file_content.len + number_string.len], &hash_buffer, .{});

        if (hash_buffer[0] == 0 and hash_buffer[1] == 0) {
            if (res.first == 0 and hash_buffer[2] < 16) {
                res.first = idx;
            }

            if (res.second == 0 and hash_buffer[2] == 0) {
                res.second = idx;
                break;
            }
        }
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
