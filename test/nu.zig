const std = @import("std");
const api_translator = @import("api-translator.zig");
const c = @cImport({
    @cInclude("nu.c");
});

pub const Error = error{
    Generic,
    File,
    InvalidArg,
    Unexpected,
    NullPointer,
};
pub const ErrorEnum = enum(c_int) {
    none,
    generic,
    file,
    invalid_arg,
    _,
};

pub const get: fn () Error!*@This() = c_to_zig.translate("nu_get", true, false, .{});

pub const hello: fn (
    handle: *@This(),
) Error!void = c_to_zig.translate("nu_hello", true, false, .{});

pub const Struct = extern struct {
    number: c_int,
    options: Options,
};

pub const world: fn (
    handle: *@This(),
    options: Options,
) Error!void = c_to_zig.translate("nu_world", true, false, .{});

pub const foo: fn (
    handle: *@This(),
    options: Options,
) Struct = c_to_zig.translate("nu_foo", false, false, .{});

pub const Options = packed struct(u16) {
    one: bool = false,
    two: bool = false,
    three: bool = false,
    _: std.meta.Int(.unsigned, @bitSizeOf(u16) - 3) = 0,
};
const c_to_zig = api_translator.Translator(.{
    .c_import_ns = c,
    .substitutions = &.{
        .{ .old = ?*c.nu, .new = *@This() },
        .{ .old = c.nu_struct, .new = Struct },
        .{ .old = u16, .new = Options },
    },
    .error_scheme = api_translator.BasicErrorScheme(ErrorEnum, Error, Error.Unexpected, .{
        .{ .type = *@This(), .err_value = null, .err = error.NullPointer },
    }),
});

test {
    inline for (comptime std.meta.declarations(@This())) |decl| {
        _ = @field(@This(), decl.name);
    }
}
