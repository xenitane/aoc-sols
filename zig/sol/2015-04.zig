const Result = lib.Result(u64, u64);
pub fn solve(_: std.mem.Allocator, file_content: []const u8) !Result {
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
const lib = @import("../extra/lib.zig");
