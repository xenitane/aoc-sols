const std = @import("std");
const Build = std.Build;

pub fn build(b: *Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{ .preferred_optimize_mode = .ReleaseFast });
    const exe_name = b.option([]const u8, "exe", "") orelse @panic("needed");

    const exe_mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const exe = b.addExecutable(.{ .root_module = exe_mod, .name = exe_name });

    b.installArtifact(exe);
    const run_exe_step = b.addRunArtifact(exe);
    run_exe_step.step.dependOn(b.getInstallStep());

    const run_step = b.step("run", "run the built executable");

    run_step.dependOn(&run_exe_step.step);

    const run_exe_unit_test_step = b.addRunArtifact(b.addTest(.{ .root_module = exe_mod }));
    b.step("test", "test the solution").dependOn(&run_exe_unit_test_step.step);
}
