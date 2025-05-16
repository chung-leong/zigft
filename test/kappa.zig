const std = @import("std");
const api_translator = @import("api-translator.zig");
const c = @cImport({
    @cInclude("kappa.c");
});

pub const Error = error{
    Failure,
    InvalidArg,
    Unexpected,
};
pub const Status = enum(c_int) {
    ok = 0,
    failure = -1,
    invalid_arg = -2,
    unexpected = -3,
    _,
};

pub const doStuff: fn () Error!void = c_to_zig.translate("kappa_do_stuff", true, false, .{});

pub const getLastStatus: fn () Status = c_to_zig.translate("kappa_get_last_status", false, false, .{ .retval = Status });

const c_to_zig = api_translator.Translator(.{
    .c_import_ns = c,
    .error_scheme = api_translator.BasicErrorScheme(Status, Error, Error.Unexpected, .{}),
});

test {
    inline for (comptime std.meta.declarations(@This())) |decl| {
        _ = @field(@This(), decl.name);
    }
}
