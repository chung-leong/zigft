const std = @import("std");
const api_translator = @import("api-translator.zig");
const c = @cImport({
    @cInclude("gamma.c");
});

pub const Error = error{
    Unexpected,
};

pub const acceptInt: fn (
    arg0: c_int,
) Error!void = c_to_zig.translate("gamma_accept_int", true, false, .{});

pub const getInt: fn () c_int = c_to_zig.translate("gamma_get_int", false, false, .{});

pub const fail: fn () Error!void = c_to_zig.translate("gamma_fail", true, false, .{});

const c_to_zig = api_translator.Translator(.{
    .c_import_ns = c,
    .error_scheme = api_translator.BasicErrorScheme(bool, Error, Error.Unexpected),
});

test {
    inline for (comptime std.meta.declarations(@This())) |decl| {
        _ = @field(@This(), decl.name);
    }
}
