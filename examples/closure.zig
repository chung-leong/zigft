const std = @import("std");
const fn_binding = @import("zigft/fn-binding.zig");

pub fn main() !void {
    var funcs: [5]*const fn (i32) void = undefined;
    for (&funcs, 0..) |*ptr, index|
        ptr.* = try fn_binding.close(struct {
            number: usize,

            pub fn print(self: @This(), arg: i32) void {
                std.debug.print("Hello: {d}, {d}\n", .{ self.number, arg });
            }
        }, .{ .number = index });
    defer for (funcs) |f| fn_binding.unbind(f);
    for (funcs) |f| f(123);
}
