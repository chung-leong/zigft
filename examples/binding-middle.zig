const std = @import("std");
const fn_binding = @import("zigft/fn-binding.zig");

pub fn main() !void {
    const ns = struct {
        fn hello(a: i8, b: i16, c: i32, d: i64) void {
            std.debug.print("a = {d}, b = {d}, c = {d}, d = {d}\n", .{ a, b, c, d });
        }
    };
    const func1 = try fn_binding.bind(ns.hello, .{ .@"2" = 300 });
    defer fn_binding.unbind(func1);
    func1(1, 2, 4);
    const func2 = try fn_binding.bind(ns.hello, .{ .@"-2" = 301 });
    defer fn_binding.unbind(func2);
    func2(1, 2, 4);
}
