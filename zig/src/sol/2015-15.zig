const Ingredient = @Vector(5, isize);
const IngredientStat = enum(u8) {
    capacity = 0,
    durability = 1,
    flavor = 2,
    texture = 3,
    calories = 4,
};

const SPOONS = 100;
const TARGET_CALORIES = 500;

const Result = lib.Result(isize, isize);
pub fn solve(ac: std.mem.Allocator, file_content: []const u8) !Result {
    var res = Result{ .first = 0, .second = 0 };

    var ingredients = std.ArrayListUnmanaged(Ingredient).empty;
    defer ingredients.deinit(ac);

    var lines = std.mem.tokenizeScalar(u8, file_content, '\n');
    while (lines.next()) |line| {
        var ingredient: Ingredient = undefined;
        var toks = std.mem.tokenizeAny(u8, line, " ,");
        _ = toks.next();
        while (toks.next()) |name| {
            const qty = try std.fmt.parseInt(isize, toks.next().?, 10);
            inline for (std.meta.fields(IngredientStat)) |field| {
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
            if (cur[4] == TARGET_CALORIES) {
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
const lib = @import("../extra/lib.zig");
