const std = @import("std");

pub fn Result(comptime U: type, comptime V: type) type {
    return struct {
        first: U,
        second: V,

        const Self = @This();

        pub fn print(self: Self, writer: *std.Io.Writer) !void {
            try print_value(U, writer, self.first);
            try print_value(V, writer, self.second);
        }
    };
}

fn print_value(comptime T: type, writer: *std.Io.Writer, value: T) !void {
    switch (@typeInfo(T)) {
        .int => {
            try writer.print("{d}\n", .{value});
        },
        .array => |info| {
            if (info.child == u8) {
                try writer.writeAll(&value);
                try writer.writeByte('\n');
            }
        },
        .pointer => |info| {
            if (info.size == std.builtin.Type.Pointer.Size.slice and info.child == u8) {
                try writer.writeAll(value);
                try writer.writeByte('\n');
            }
        },
        else => {},
    }
}

pub fn readEntireFile(ac: std.mem.Allocator, file_name: []const u8) ![]const u8 {
    const file = try std.fs.cwd().openFile(file_name, .{});
    defer file.close();
    const file_size = (try file.stat()).size;
    return try file.readToEndAlloc(ac, file_size + 10);
}
