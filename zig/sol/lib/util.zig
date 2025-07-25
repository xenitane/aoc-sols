const std = @import("std");

pub fn readEntireFile(ac: std.mem.Allocator, file_name: []const u8) ![]const u8 {
    const file = try std.fs.cwd().openFile(file_name, .{});
    defer file.close();
    const file_size = (try file.stat()).size;
    return try file.readToEndAlloc(ac, file_size + 10);
}
