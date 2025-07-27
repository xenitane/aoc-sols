const Variable = union(enum) {
    raw: u16,
    iden: []const u8,
};

const Expression = union(enum) {
    NOP: Variable,
    NOT: Variable,
    AND: [2]Variable,
    OR: [2]Variable,
    LSHIFT: [2]Variable,
    RSHIFT: [2]Variable,
};

const ExpressionMap = std.StringArrayHashMapUnmanaged(Expression);
const ValueMap = std.StringArrayHashMapUnmanaged(u16);

const Result = lib.Result(u16, u16);
fn solve(ac: std.mem.Allocator, file_content: []const u8) !Result {
    var res = Result{ .first = undefined, .second = undefined };

    var values = ValueMap.empty;
    defer values.deinit(ac);
    var expressions = ExpressionMap.empty;
    defer expressions.deinit(ac);

    var lines = std.mem.tokenizeScalar(u8, file_content, '\n');
    while (lines.next()) |line| {
        var toks = std.mem.tokenizeScalar(u8, line, ' ');
        const ft = toks.next().?;
        if (std.mem.eql(u8, ft, "NOT")) {
            const ivar0 = try makeVar(toks.next().?);
            _ = toks.next();
            try expressions.put(ac, toks.next().?, .{ .NOT = ivar0 });
            continue;
        }
        const ivar0 = try makeVar(ft);
        const op = toks.next().?;
        if (std.mem.eql(u8, op, "->")) {
            try expressions.put(ac, toks.next().?, .{ .NOP = ivar0 });
            continue;
        }
        const ivar1 = try makeVar(toks.next().?);
        _ = toks.next();

        inline for (std.meta.fields(Expression)) |field| {
            if (std.mem.eql(u8, field.name, op) and field.type == [2]Variable) {
                try expressions.put(ac, toks.next().?, @unionInit(Expression, field.name, .{ ivar0, ivar1 }));
                break;
            }
        }
    }

    res.first = try solveExpression(ac, &expressions, &values, "a");

    values.clearAndFree(ac);
    try expressions.put(ac, "b", .{ .NOP = .{ .raw = res.first } });

    res.second = try solveExpression(ac, &expressions, &values, "a");

    return res;
}

fn solveExpression(ac: std.mem.Allocator, exp_map: *ExpressionMap, val_map: *ValueMap, var_name: []const u8) anyerror!u16 {
    if (val_map.get(var_name)) |res| {
        return res;
    }

    const res = switch (exp_map.get(var_name).?) {
        .NOP => |val| try unwrapVar(ac, exp_map, val_map, val),
        .NOT => |val| ~(try unwrapVar(ac, exp_map, val_map, val)),
        .AND => |val| (try unwrapVar(ac, exp_map, val_map, val[0])) & (try unwrapVar(ac, exp_map, val_map, val[1])),
        .OR => |val| (try unwrapVar(ac, exp_map, val_map, val[0])) | (try unwrapVar(ac, exp_map, val_map, val[1])),
        .LSHIFT => |val| (try unwrapVar(ac, exp_map, val_map, val[0])) << @as(u4, @truncate(try unwrapVar(ac, exp_map, val_map, val[1]))),
        .RSHIFT => |val| (try unwrapVar(ac, exp_map, val_map, val[0])) >> @as(u4, @truncate(try unwrapVar(ac, exp_map, val_map, val[1]))),
    };
    try val_map.put(ac, var_name, res);
    return res;
}

fn unwrapVar(ac: std.mem.Allocator, exp_map: *ExpressionMap, val_map: *ValueMap, var_var: Variable) !u16 {
    return switch (var_var) {
        .raw => |raw| raw,
        .iden => |iden| try solveExpression(ac, exp_map, val_map, iden),
    };
}

fn makeVar(str: []const u8) !Variable {
    if (std.ascii.isDigit(str[0])) {
        return Variable{ .raw = try std.fmt.parseInt(u16, str, 10) };
    }
    return Variable{ .iden = str };
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
