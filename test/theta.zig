const std = @import("std");
const api_translator = @import("api-translator.zig");
const c = @cImport({
    @cInclude("theta.c");
});

pub const Error = error{
    Failure,
    InvalidArg,
    Unexpected,
    WrongDwarf,
};
pub const Status = enum(c_uint) {
    ok,
    failure,
    invalid_arg,
    unexpected,
    wrong_dwarf,
    _,
};
pub const Flags = packed struct(c_uint) {
    happy: bool = false,
    grumpy: bool = false,
    sleepy: bool = false,
    dopey: bool = false,
    bashful: bool = false,
    sneezy: bool = false,
    _: std.meta.Int(.unsigned, @bitSizeOf(c_uint) - 6) = 0,
};
pub const Callback = *const fn (
    Flags,
) callconv(.c) void;

pub const isSleepy: fn (
    flags: Flags,
) Error!void = c_to_zig.translate("theta_is_sleepy", true, false, .{});

pub const setDwarves: fn (
    flags: Flags,
) Error!void = c_to_zig.translate("theta_set_dwarves", true, false, .{});

pub const invoke: fn (
    cb: Callback,
) void = c_to_zig.translate("theta_invoke", false, false, .{});

const c_to_zig = api_translator.Translator(.{
    .c_import_ns = c,
    .substitutions = &.{
        .{ .old = c.theta_callback, .new = Callback },
        .{ .old = c.theta_flags, .new = Flags },
    },
    .error_scheme = api_translator.BasicErrorScheme(Status, Error, Error.Unexpected),
});

test {
    inline for (comptime std.meta.declarations(@This())) |decl| {
        _ = @field(@This(), decl.name);
    }
}
