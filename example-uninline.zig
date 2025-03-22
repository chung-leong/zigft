const std = @import("std");
const fn_transform = @import("./fn-transform.zig");

pub fn Uninlined(comptime FT: type) type {
    return switch (@typeInfo(FT)) {
        .@"fn" => |f| @Type(.{
            .@"fn" = .{
                .calling_convention = switch (f.calling_convention) {
                    .@"inline" => .auto,
                    else => |cc| cc,
                },
                .is_generic = f.is_generic,
                .is_var_args = f.is_var_args,
                .return_type = f.return_type,
                .params = f.params,
            },
        }),
        else => @compileError("Not a function"),
    };
}

fn uninline(comptime func: anytype) Uninlined(@TypeOf(func)) {
    const FT = @TypeOf(func);
    const ns = struct {
        inline fn call(args: std.meta.ArgsTuple(FT)) @typeInfo(FT).@"fn".return_type.? {
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
