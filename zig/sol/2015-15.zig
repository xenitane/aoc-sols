const Ingredient = @Vector(5, isize);
const IngredientStat = enum(u8) {
    capacity = 0,
    durability = 1,
    flavor = 2,
    texture = 3,
    calories = 4,
};

const Result = lib.Result(isize, isize);
fn solve(ac: std.mem.Allocator, file_content: []const u8) !Result {
    var res = Result{ .first = 0, .second = 0 };

    const SPOONS = 100;

    var ingredients = std.ArrayListUnmanaged(Ingredient).empty;
    defer ingredients.deinit(ac);

    var lines = std.mem.tokenizeScalar(u8, file_content, '\n');
    while (lines.next()) |line| {
        var ingredient: Ingredient = undefined;
        var toks = std.mem.tokenizeAny(u8, line, " ,");
        _ = toks.next();
        while (toks.next()) |name| {
            const qty = try std.fmt.parseInt(isize, toks.next().?, 10);
            inline for (@typeInfo(IngredientStat).@"enum".fields) |field| {
                if (std.mem.eql(u8, field.name, name)) {
                    ingredient[field.value] = qty;
                    break;
                }
            }
        }
        try ingredients.append(ac, ingredient);
    }

    findMaxScore(ingredients.items, SPOONS, Ingredient{ 0, 0, 0, 0, 0 }, &res.first, &res.second);

    return res;
}

fn findMaxScore(ingredients: []const Ingredient, spoons: usize, cur: Ingredient, first: *isize, second: *isize) void {
    if (0 == ingredients.len) {
        if (0 != spoons) {
            return;
        }
        const value = blk: {
            var prod: isize = 1;
            for (0..4) |i| {
                prod *= @max(0, cur[i]);
            }
            break :blk prod;
        };
        if (value > 0) {
            first.* = @max(first.*, value);
            if (cur[4] == 500) {
                second.* = @max(second.*, value);
            }
        }
        return;
    }
    for (0..spoons + 1) |j| {
        findMaxScore(ingredients[1..], spoons - j, cur + scaleVec(ingredients[0], @bitCast(j)), first, second);
    }
}

inline fn scaleVec(ing: Ingredient, scale: isize) Ingredient {
    return Ingredient{
        ing[0] * scale,
        ing[1] * scale,
        ing[2] * scale,
        ing[3] * scale,
        ing[4] * scale,
    };
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
