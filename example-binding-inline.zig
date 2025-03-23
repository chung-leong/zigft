const std = @import("std");
const fn_binding = @import("./fn-binding.zig");

pub fn main() !void {
    const ns = struct {
        inline fn hello(a: i32, b: i32) void {
            std.debug.print("sum = {d}\n", .{a + b});
        }
    };
    const func = try fn_binding.bind(ns.hello, .{ .@"-1" = 123 });
    func(3);
}
