const std = @import("std");
const fn_binding = @import("./fn-binding.zig");

pub fn main() !void {
    var funcs: [5]*const fn () void = undefined;
    for (&funcs, 0..) |*ptr, index|
        ptr.* = try fn_binding.bind(std.debug.print, .{ "hello: {d}\n", .{index + 1} });
    defer for (funcs) |f| fn_binding.unbind(f);
    for (funcs) |f| f();
}
