const std = @import("std");
const fn_transform = @import("./fn-transform.zig");

fn Uninlined(comptime FT: type) type {
    const f = @typeInfo(FT).@"fn";
    if (f.calling_convention != .@"inline") return FT;
    return @Type(.{
        .@"fn" = .{
            .calling_convention = .auto,
            .is_generic = f.is_generic,
            .is_var_args = f.is_var_args,
            .return_type = f.return_type,
            .params = f.params,
        },
    });
}

fn uninline(func: anytype) Uninlined(@TypeOf(func)) {
    const FT = @TypeOf(func);
    const f = @typeInfo(FT).@"fn";
    if (f.calling_convention != .@"inline") return func;
    const ns = struct {
        inline fn call(args: std.meta.ArgsTuple(FT)) f.return_type.? {
            return @call(.auto, func, args);
        }
    };
    return fn_transform.spreadArgs(ns.call, .auto);
}

pub fn main() void {
    const ns = struct {
        inline fn hello(a: i32, b: i32) void {
            std.debug.print("sum = {d}\n", .{a + b});
        }
    };
    const func = uninline(ns.hello);
    std.debug.print("fn address = {x}\n", .{@intFromPtr(&func)});
}
