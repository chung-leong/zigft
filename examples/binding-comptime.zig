const std = @import("std");
const fn_binding = @import("zigft/fn-binding.zig");

pub fn main() !void {
    const ns = struct {
        const dog = fn_binding.bind(std.debug.print, .{ "Woof!\n", .{} }) catch unreachable;
        const cat = fn_binding.bind(std.debug.print, .{ "Meow!\n", .{} }) catch unreachable;
        const fox = fn_binding.define(std.debug.print, .{ "???\n", .{} });
    };
    ns.dog();
    ns.cat();
    ns.fox();
}
