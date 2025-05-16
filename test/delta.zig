const std = @import("std");
const api_translator = @import("api-translator.zig");
const c = @cImport({
    @cInclude("delta.c");
});

pub const Error = error{
    Unexpected,
};

pub const acceptInt: fn (
    arg0: c_int,
) Error!void = c_to_zig.translate("delta_accept_int", true, false, .{});

pub const fail: fn () Error!void = c_to_zig.translate("delta_fail", true, false, .{});

const c_to_zig = api_translator.Translator(.{
    .c_import_ns = c,
    .error_scheme = api_translator.BasicErrorScheme(c_int, Error, Error.Unexpected, .{}),
});

test {
    inline for (comptime std.meta.declarations(@This())) |decl| {
        _ = @field(@This(), decl.name);
    }
}
