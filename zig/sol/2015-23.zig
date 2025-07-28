const InstructionKind = enum { hlf, tpl, inc, jmp, jie, jio };
const Reg = enum { a, b };
const Instruction = struct {
    kind: InstructionKind,
    reg: ?Reg = null,
    jump_offset: isize = 0,
};

const Computer = struct {
    a: usize = 0,
    b: usize = 0,

    ip: isize = 0,

    instructions: []const Instruction,

    fn run(self: *Computer) void {
        while (self.ip >= 0 and @as(usize, @bitCast(self.ip)) < self.instructions.len) {
            const ip = @as(usize, @bitCast(self.ip));
            const instruction = self.instructions[ip];
            const reg = switch (instruction.reg orelse .a) {
                .a => &self.a,
                .b => &self.b,
            };
            var jo: isize = 1;
            switch (instruction.kind) {
                .hlf => reg.* >>= 1,
                .tpl => reg.* *= 3,
                .inc => reg.* += 1,
                .jmp => jo = instruction.jump_offset,
                .jie => jo = switch (reg.* & 1) {
                    0 => instruction.jump_offset,
                    else => 1,
                },
                .jio => jo = switch (reg.*) {
                    1 => instruction.jump_offset,
                    else => 1,
                },
            }
            self.ip += jo;
        }
    }
    fn reset(self: *Computer) void {
        self.* = Computer{ .instructions = self.instructions };
    }
};

const Result = lib.Result(usize, usize);
fn solve(ac: std.mem.Allocator, file_content: []const u8) !Result {
    var res = Result{ .first = undefined, .second = undefined };

    var instructions = std.ArrayListUnmanaged(Instruction).empty;
    defer instructions.deinit(ac);

    var lines = std.mem.tokenizeScalar(u8, file_content, '\n');
    while (lines.next()) |line| {
        var toks = std.mem.tokenizeAny(u8, line, ", ");
        const label = toks.next().?;
        if (std.mem.eql(u8, label, "jmp")) {
            const offset = try std.fmt.parseInt(isize, toks.next().?, 10);
            try instructions.append(ac, .{
                .kind = .jmp,
                .jump_offset = offset,
            });
            continue;
        }
        inline for (std.meta.fields(InstructionKind)) |field| {
            if (std.mem.eql(u8, label, field.name)) {
                const reg = switch (std.mem.eql(u8, toks.next().?, "a")) {
                    true => Reg.a,
                    false => Reg.b,
                };
                var jmp_offset: isize = 0;
                if (label[0] == 'j') {
                    jmp_offset = try std.fmt.parseInt(isize, toks.next().?, 10);
                }

                try instructions.append(ac, .{
                    .kind = @field(InstructionKind, field.name),
                    .reg = reg,
                    .jump_offset = jmp_offset,
                });
                break;
            }
        }
    }

    var computer = Computer{ .instructions = instructions.items };

    computer.run();
    res.first = computer.b;

    computer.reset();
    computer.a = 1;

    computer.run();
    res.second = computer.b;

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
