const std = @import("std");
const fn_transform = @import("zigft/fn-transform.zig");

pub fn Uninlined(comptime FT: type) type {
    const f = @typeInfo(FT).@"fn";
    if (f.calling_convention != .@"inline") return FT;
    var param_types: [f.params.len]type = undefined;
    var param_attrs: [f.params.len]std.builtin.Type.Fn.Param.Attributes = undefined;
    inline for (f.params, 0..) |param, i| {
        param_types[i] = param.type.?;
        param_attrs[i] = .{ .@"noalias" = param.is_noalias };
    }
    return @Fn(&param_types, &param_attrs, f.return_type.?, .{
        .varargs = f.is_var_args,
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
