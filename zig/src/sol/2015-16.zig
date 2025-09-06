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

const stat_names = .{
    "pomeranians", "goldfish",
    "children",    "samoyeds",
    "akitas",      "vizslas",
    "cars",        "perfumes",
    "cats",        "trees",
};
const stat_ops = .{
    std.math.Order.lt, std.math.Order.lt,
    std.math.Order.eq, std.math.Order.eq,
    std.math.Order.eq, std.math.Order.eq,
    std.math.Order.eq, std.math.Order.eq,
    std.math.Order.gt, std.math.Order.gt,
};

const default_stats = Stat{
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

const Result = lib.Result(usize, usize);
pub fn solve(_: std.mem.Allocator, file_content: []const u8) !Result {
    var res = Result{ .first = 0, .second = 0 };

    var lines = std.mem.tokenizeScalar(u8, file_content, '\n');

    while (lines.next()) |line| {
        var toks = std.mem.tokenizeAny(u8, line, ": ,");
        var stat = Stat{};
        _, const id = .{ toks.next(), try std.fmt.parseInt(u32, toks.next().?, 10) };
        while (toks.next()) |name| {
            inline for (std.meta.fields(Stat)) |field| {
                if (std.mem.eql(u8, name, field.name)) {
                    @field(stat, field.name) = try std.fmt.parseInt(u32, toks.next().?, 10);
                    break;
                }
            }
        }

        var found0 = true;
        var found1 = true;
        inline for (stat_names, stat_ops) |field_name, ord| {
            if (@field(stat, field_name)) |val| {
                const actual_val = @field(default_stats, field_name).?;
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
const lib = @import("../extra/lib.zig");
