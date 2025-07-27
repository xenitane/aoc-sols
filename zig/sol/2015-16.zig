const Stat = struct {
    children: ?u32 = null,
    cats: ?u32 = null,
    samoyeds: ?u32 = null,
    pomeranians: ?u32 = null,
    akitas: ?u32 = null,
    vizslas: ?u32 = null,
    goldfish: ?u32 = null,
    trees: ?u32 = null,
    cars: ?u32 = null,
    perfumes: ?u32 = null,
};

const Result = lib.Result(usize, usize);
fn solve(_: std.mem.Allocator, file_content: []const u8) !Result {
    var res = Result{ .first = 0, .second = 0 };

    var lines = std.mem.tokenizeScalar(u8, file_content, '\n');

    const def = Stat{
        .children = 3,
        .cats = 7,
        .samoyeds = 2,
        .pomeranians = 3,
        .akitas = 0,
        .vizslas = 0,
        .goldfish = 5,
        .trees = 3,
        .cars = 2,
        .perfumes = 1,
    };
    while (lines.next()) |line| {
        var toks = std.mem.tokenizeAny(u8, line, ": ,");
        var stat = Stat{};
        _, const id = .{ toks.next(), try std.fmt.parseInt(u32, toks.next().?, 10) };
        while (toks.next()) |name| {
            inline for (@typeInfo(Stat).@"struct".fields) |field| {
                if (std.mem.eql(u8, name, field.name)) {
                    @field(stat, field.name) = try std.fmt.parseInt(u32, toks.next().?, 10);
                    break;
                }
            }
        }

        var found0 = true;
        var found1 = true;
        inline for (.{
            "pomeranians", "goldfish",
            "children",    "samoyeds",
            "akitas",      "vizslas",
            "cars",        "perfumes",
            "cats",        "trees",
        }, [_]std.math.Order{
            .lt, .lt,
            .eq, .eq,
            .eq, .eq,
            .eq, .eq,
            .gt, .gt,
        }) |field_name, ord| {
            if (@field(stat, field_name)) |val| {
                const actual_val = @field(def, field_name).?;
                if (val != actual_val) {
                    found0 = false;
                }
                if (std.math.order(val, actual_val) != ord) {
                    found1 = false;
                }
            }
        }
        if (found0) {
            res.first = id;
        }
        if (found1) {
            res.second = id;
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
