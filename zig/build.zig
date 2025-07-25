const std = @import("std");
const Build = std.Build;

pub fn build(b: *Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{ .preferred_optimize_mode = .ReleaseFast });

    var dir = try std.fs.cwd().openDir("sol", .{ .iterate = true });
    defer dir.close();
    var it = dir.iterate();
    while (try it.next()) |entry| {
        if (entry.kind != .file or !std.mem.endsWith(u8, entry.name, ".zig")) {
            continue;
        }
        const base = entry.name[0 .. entry.name.len - 4];
        const file_path = b.pathJoin(&.{ "sol", entry.name });

        const opts = b.addOptions();
        opts.addOption([]const u8, "input_file_path", b.fmt("../inputs/{s}.txt", .{base}));
        opts.addOption([]const u8, "test_input_file_path", b.fmt("../test_inputs/{s}.txt", .{base}));
        opts.addOption([]const u8, "test_output_file_path", b.fmt("../test_outputs/{s}.txt", .{base}));

        const exe_mod = b.createModule(.{
            .root_source_file = b.path(file_path),
            .target = target,
            .optimize = optimize,
        });
        exe_mod.addImport("opts", opts.createModule());

        {
            const run_exe_step = b.addRunArtifact(b.addExecutable(.{ .root_module = exe_mod, .name = base }));
            const step_name = b.fmt("run-{s}", .{base});
            b.step(step_name, step_name).dependOn(&run_exe_step.step);
        }
        {
            const run_exe_unit_test_step = b.addRunArtifact(b.addTest(.{ .root_module = exe_mod }));
            const step_name = b.fmt("test-{s}", .{base});
            b.step(step_name, step_name).dependOn(&run_exe_unit_test_step.step);
        }
    }
}
