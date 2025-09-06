const Equipment = @Vector(3, u16);

const WEAPONS = [_]Equipment{
    .{ 8, 4, 0 },
    .{ 10, 5, 0 },
    .{ 25, 6, 0 },
    .{ 40, 7, 0 },
    .{ 74, 8, 0 },
};

const ARMOR = [_]Equipment{
    .{ 13, 0, 1 },
    .{ 31, 0, 2 },
    .{ 53, 0, 3 },
    .{ 75, 0, 4 },
    .{ 102, 0, 5 },
};

const RINGS = [_]Equipment{
    .{ 25, 1, 0 },
    .{ 50, 2, 0 },
    .{ 100, 3, 0 },
    .{ 20, 0, 1 },
    .{ 40, 0, 2 },
    .{ 80, 0, 3 },
};

const PlayerEquipment = struct {
    a: u8 = 0,
    b: ?u8 = null,
    c: [2]?u8 = .{ null, null },

    fn val(self: PlayerEquipment) Equipment {
        var res: Equipment = WEAPONS[self.a];
        if (self.b) |_b| {
            res += ARMOR[_b];
        }
        if (self.c[0]) |_c| {
            res += RINGS[_c];
            if (self.c[1]) |__c| {
                res += RINGS[__c];
            }
        }
        return res;
    }

    fn next(self: *PlayerEquipment) bool {
        if (null == self.c[0]) {
            self.c[0] = 0;
            return true;
        }
        if (null == self.c[1] and self.c[0].? + 1 < RINGS.len) {
            self.c[1] = self.c[0].? + 1;
            return true;
        }
        if (self.c[1]) |*v| {
            if (v.* + 1 < RINGS.len) {
                v.* += 1;
            } else {
                self.c[0].? += 1;
                self.c[1] = null;
            }
            return true;
        }
        self.c = .{ null, null };
        if (null == self.b) {
            self.b = 0;
            return true;
        }
        if (self.b.? + 1 < ARMOR.len) {
            self.b.? += 1;
            return true;
        }
        self.b = null;
        self.a += 1;
        return self.a < WEAPONS.len;
    }
};

const PlayerStat = struct {
    @"Hit Points": u16 = 100,
    Damage: u16 = 0,
    Armor: u16 = 0,
};

const Result = lib.Result(usize, usize);
pub fn solve(_: std.mem.Allocator, file_content: []const u8) !Result {
    var res = Result{ .first = std.math.maxInt(usize), .second = 0 };

    const boss_stats = blk: {
        var boss_stats = PlayerStat{};
        var lines = std.mem.tokenizeScalar(u8, file_content, '\n');
        while (lines.next()) |line| {
            var toks = std.mem.tokenizeSequence(u8, line, ": ");
            const label, const val = .{ toks.next().?, try std.fmt.parseInt(u16, toks.next().?, 10) };
            inline for (std.meta.fields(PlayerStat)) |field| {
                if (std.mem.eql(u8, field.name, label)) {
                    @field(boss_stats, field.name) = val;
                    break;
                }
            }
        }
        break :blk boss_stats;
    };

    var player_equipment = PlayerEquipment{};

    while (true) {
        var player_stats = PlayerStat{};
        const cost, player_stats.Damage, player_stats.Armor = player_equipment.val();

        if (conductMatch(player_stats, boss_stats)) {
            res.first = @min(res.first, cost);
        } else {
            res.second = @max(res.second, cost);
        }

        if (!player_equipment.next()) break;
    }

    return res;
}

inline fn conductMatch(p0: PlayerStat, p1: PlayerStat) bool {
    const p0_attack = @max(p0.Damage, p1.Armor + 1) - p1.Armor;
    const p0_moves = @divFloor((p1.@"Hit Points" + p0_attack - 1), p0_attack);

    const p1_attack = @max(p1.Damage, p0.Armor + 1) - p0.Armor;
    const p1_moves = @divFloor(p0.@"Hit Points" + p1_attack - 1, p1_attack);

    return p0_moves <= p1_moves;
}

const std = @import("std");
const lib = @import("../extra/lib.zig");
