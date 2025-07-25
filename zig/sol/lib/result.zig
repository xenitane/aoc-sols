const std = @import("std");

pub fn Result(comptime U: type, comptime V: type) type {
    return struct {
        first: U,
        second: V,

        const Self = @This();

        pub fn print(self: Self, writer: anytype) !void {
            try print_value(U, writer, self.first);
            try print_value(V, writer, self.second);
        }
    };
}

fn print_value(comptime T: type, writer: anytype, value: T) !void {
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
