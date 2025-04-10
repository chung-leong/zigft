const std = @import("std");
const api_translator = @import("api-translator.zig");
const c = @cImport({
    @cInclude("zeta.c");
});

pub const Error = error{
    Failure,
    InvalidArg,
    Unexpected,
};
pub const Status = enum(c_uint) {
    ok,
    failure,
    invalid_arg,
    unexpected,
    _,
};
pub const Callback1 = *const fn () callconv(.c) void;
pub const Callback2 = *const fn (
    c_int,
) callconv(.c) Status;

pub const setCallback1: fn (
    f: Callback1,
) Error!void = c_to_zig.translate("zeta_set_callback1", true, false, .{});

pub const call1: fn () Error!void = c_to_zig.translate("zeta_call1", true, false, .{});

pub const setCallback2: fn (
    f: Callback2,
) Error!void = c_to_zig.translate("zeta_set_callback2", true, false, .{});

pub const call2: fn (
    arg: c_int,
) Error!void = c_to_zig.translate("zeta_call2", true, false, .{});

const c_to_zig = api_translator.Translator(.{
    .c_import_ns = c,
    .substitutions = &.{
        .{ .old = c.zeta_callback1, .new = Callback1 },
        .{ .old = c.zeta_callback2, .new = Callback2 },
    },
    .error_scheme = api_translator.BasicErrorScheme(Status, Error, Error.Unexpected),
});

test {
    inline for (comptime std.meta.declarations(@This())) |decl| {
        _ = @field(@This(), decl.name);
    }
}
