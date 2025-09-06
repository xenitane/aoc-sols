const Spell = union(enum) {
    cast: struct {
        damage: u16,
        heal: u16,
    },
    effect: struct {
        timer: u16,
        val: u16,
        kind: enum { DAMAGE, ARMOR, MANA },
    },

    fn start(self: *Spell, m: *Player, _: *Player) void {
        if (self.* == .effect and self.effect.kind == .ARMOR) {
            m.armor += self.effect.val;
        }
    }
    fn apply(self: *Spell, m: *Player, o: *Player) void {
        switch (self.*) {
            .cast => |info| {
                m.@"hit points" += info.heal;
                o.applyDamage(info.damage);
            },
            .effect => |*info| {
                switch (info.kind) {
                    .DAMAGE => {
                        o.applyDamage(info.val);
                    },
                    .MANA => {
                        m.mana += info.val;
                    },
                    .ARMOR => {},
                }
                info.timer -= 1;
            },
        }
    }
    fn end(self: *Spell, m: *Player, _: *Player) void {
        if (self.* == .effect and self.effect.kind == .ARMOR) {
            m.armor -= self.effect.val;
        }
    }
};

const SpellExt = struct {
    cost: u16,
    spell: Spell,
};

const Player = struct {
    @"hit points": u16 = 50,
    armor: u16 = 0,
    damage: u16 = 0,
    mana: u16 = 500,

    fn applyDamage(self: *Player, n: u16) void {
        self.@"hit points" -|= @max(n, self.armor + 1) - self.armor;
    }
};

const AVAILABLESPELLS = [_]SpellExt{
    .{ .cost = 113, .spell = .{ .effect = .{ .timer = 6, .val = 7, .kind = .ARMOR } } },
    .{ .cost = 173, .spell = .{ .effect = .{ .timer = 6, .val = 3, .kind = .DAMAGE } } },
    .{ .cost = 229, .spell = .{ .effect = .{ .timer = 5, .val = 101, .kind = .MANA } } },
    .{ .cost = 53, .spell = .{ .cast = .{ .damage = 4, .heal = 0 } } },
    .{ .cost = 73, .spell = .{ .cast = .{ .damage = 2, .heal = 2 } } },
};

const Result = lib.Result(usize, usize);
pub fn solve(_: std.mem.Allocator, file_content: []const u8) !Result {
    var res = Result{ .first = std.math.maxInt(usize), .second = std.math.maxInt(usize) };

    const boss = blk: {
        var boss = Player{};
        var lines = std.mem.tokenizeScalar(u8, file_content, '\n');
        while (lines.next()) |line| {
            var toks = std.mem.tokenizeSequence(u8, line, ": ");
            const label, const val = .{ toks.next().?, try std.fmt.parseInt(u16, toks.next().?, 10) };
            inline for (std.meta.fields(Player)) |field| {
                if (std.ascii.eqlIgnoreCase(field.name, label)) {
                    @field(boss, field.name) = val;
                    break;
                }
            }
        }
        break :blk boss;
    };

    conductMatch(Player{}, boss, true, .{ null, null, null }, 0, false, &res.first);
    conductMatch(Player{}, boss, true, .{ null, null, null }, 0, true, &res.second);

    return res;
}

fn conductMatch(player: Player, boss: Player, my_turn: bool, active_spells: [3]?Spell, spent: u16, hard: bool, res: *usize) void {
    var new_player = player;
    if (hard and my_turn) {
        new_player.applyDamage(1);
        if (new_player.@"hit points" == 0) {
            return;
        }
    }
    new_player, const new_boss, const new_active_spells = applyEffect(new_player, boss, active_spells);
    if (new_boss.@"hit points" == 0) {
        res.* = @min(res.*, spent);
        return;
    }
    if (my_turn) {
        if (spent >= res.*) {
            return;
        }
        for (0..AVAILABLESPELLS.len) |i| {
            if ((i < 3 and null != new_active_spells[i]) or player.mana < AVAILABLESPELLS[i].cost) {
                continue;
            }
            const new_player_0, const new_boss_0, const new_active_spells_0 = activateSpell(new_player, new_boss, i, new_active_spells);
            const new_spent = spent + AVAILABLESPELLS[i].cost;
            if (new_boss_0.@"hit points" == 0) {
                res.* = @min(res.*, new_spent);
                continue;
            }
            conductMatch(new_player_0, new_boss_0, false, new_active_spells_0, new_spent, hard, res);
        }
    } else {
        new_player.applyDamage(new_boss.damage);
        if (new_player.@"hit points" == 0) {
            return;
        }
        conductMatch(new_player, new_boss, true, new_active_spells, spent, hard, res);
    }
}

inline fn activateSpell(player: Player, boss: Player, spellIdx: usize, active_spells: [3]?Spell) struct { Player, Player, [3]?Spell } {
    var new_player = player;
    var new_boss = boss;
    var new_active_spells = active_spells;

    new_player.mana -= AVAILABLESPELLS[spellIdx].cost;

    if (spellIdx < 3) {
        new_active_spells[spellIdx] = AVAILABLESPELLS[spellIdx].spell;
        new_active_spells[spellIdx].?.start(&new_player, &new_boss);
    } else {
        var spell = AVAILABLESPELLS[spellIdx].spell;
        spell.apply(&new_player, &new_boss);
    }

    return .{ new_player, new_boss, new_active_spells };
}

inline fn applyEffect(player: Player, boss: Player, active_spells: [3]?Spell) struct { Player, Player, [3]?Spell } {
    var new_player = player;
    var new_boss = boss;
    var new_active_spells = active_spells;
    for (0..new_active_spells.len) |i| {
        if (new_active_spells[i]) |*spell| {
            if (spell.* == .cast) {
                @panic("unreachable");
            }
            spell.apply(&new_player, &new_boss);
            if (spell.effect.timer == 0) {
                spell.end(&new_player, &new_boss);
                new_active_spells[i] = null;
            }
        }
    }
    return .{ new_player, new_boss, new_active_spells };
}

const std = @import("std");
const lib = @import("../extra/lib.zig");
