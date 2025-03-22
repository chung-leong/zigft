const std = @import("std");
const fn_transform = @import("./fn-transform.zig");

fn attachDebugOutput(comptime func: anytype, comptime name: []const u8) @TypeOf(func) {
    const FT = @TypeOf(func);
    const fn_info = @typeInfo(FT).@"fn";
    const ns = struct {
        inline fn call(args: std.meta.ArgsTuple(FT)) fn_info.return_type.? {
            std.debug.print("{s}: {any}\n", .{ name, args });
            return @call(.auto, func, args);
        }
    };
    return fn_transform.spreadArgs(ns.call, fn_info.calling_convention);
}

pub fn main() void {
    const ns = struct {
        fn hello(a: i32, b: i32) void {
            std.debug.print("sum = {d}\n", .{a + b});
        }
    };
    const func = attachDebugOutput(ns.hello, "hello");
    func(123, 456);
}
