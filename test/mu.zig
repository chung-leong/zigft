const std = @import("std");
const api_translator = @import("api-translator.zig");
const c = @cImport({
    @cInclude("mu.c");
});

pub const Error = error{
    Unexpected,
};

pub const getVoidPtr: fn (
    arg: bool,
) Error!*anyopaque = c_to_zig.translate("mu_get_void_ptr", true, false, .{});

pub const getIntPtr: fn (
    arg: bool,
) Error!*c_int = c_to_zig.translate("mu_get_int_ptr", true, false, .{});

pub const Handle = c_int;

pub const getHandle: fn (
    arg: bool,
) Error!Handle = c_to_zig.translate("mu_get_handle", true, false, .{});

const c_to_zig = api_translator.Translator(.{
    .c_import_ns = c,
    .substitutions = &.{
        .{ .old = ?*anyopaque, .new = *anyopaque },
        .{ .old = [*c]c_int, .new = *c_int },
    },
    .error_scheme = api_translator.InvalidReturnValueScheme(Error, Error.Unexpected, .{
        @as(Handle, c.INVALID_HANDLE),
    }),
});

test {
    inline for (comptime std.meta.declarations(@This())) |decl| {
        _ = @field(@This(), decl.name);
    }
}
