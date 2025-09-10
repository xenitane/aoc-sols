const std = @import("std");
const lib = @import("extra/lib.zig");
const Opts = @import("extra/Opts.zig");
const solve = Opts.solve;

pub fn main() !void {
    var da = std.heap.DebugAllocator(.{}).init;
    defer {
        if (da.deinit() == std.heap.Check.leak) {
            @panic("!!!memory leak detected!!!\n");
        }
    }

    const ac = da.allocator();

    const file_content = try lib.readEntireFile(ac, Opts.input_file_path);
    defer ac.free(file_content);

    const res = try solve(ac, file_content);
    var buffer: [2048]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&buffer);
    const writer = &stdout_writer.interface;
    try res.print(writer);
    try writer.flush();
}

test solve {
    const ally = std.testing.allocator;

    const in_file_content = try lib.readEntireFile(ally, Opts.test_input_file_path);
    defer ally.free(in_file_content);
    const out_file_content = try lib.readEntireFile(ally, Opts.test_output_file_path);
    defer ally.free(out_file_content);

    const res = try solve(ally, in_file_content);
    var string_builder = std.ArrayList(u8).empty;
    defer string_builder.deinit(ally);
    try res.print(string_builder.writer(ally));

    while (string_builder.getLastOrNull()) |elem| {
        if (!std.ascii.isWhitespace(elem)) {
            break;
        }
        _ = string_builder.pop();
    }

    try std.testing.expectEqualStrings(out_file_content, string_builder.items);
}
