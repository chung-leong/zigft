const std = @import("std");
const api_translator = @import("api-translator.zig");
const c = @cImport({
    @cInclude("alpha.c");
});

pub const Error = error{
    Failure,
    InvalidArg,
    Unknown,
    Unexpected,
};
pub const Status = enum(c_uint) {
    ok,
    failure,
    invalid_arg,
    unknown,
    _,
};
pub const Mood = enum(c_uint) {
    lousy,
    lazy,
    happy,
    bad,
    _,
};
pub const acceptInt: fn (
    arg0: c_int,
) Error!void = c_to_zig.translate("alpha_accept_int", true, false, .{});
pub const getInt: fn (
) c_int = c_to_zig.translate("alpha_get_int", false, false, .{});
pub const acceptEnum: fn (
    arg0: Mood,
) Error!void = c_to_zig.translate("alpha_accept_enum", true, false, .{ .@"0" = Mood });
pub const getEnum: fn (
) Mood = c_to_zig.translate("alpha_get_enum", false, false, .{ .retval = Mood });
pub const Struct = extern struct {
    big_number: c_int,
    small_number: c_int,
};
pub const acceptStruct: fn (
    arg0: *const Struct,
) Error!Struct = c_to_zig.translate("alpha_accept_struct", true, false, .{});
pub const fail: fn (
) Error!std.meta.Tuple(&.{ c_int, c_int }) = c_to_zig.translate("alpha_fail", true, false, .{});
pub const failUnknown: fn (
) Error!std.meta.Tuple(&.{ c_int, c_int }) = c_to_zig.translate("alpha_fail_unknown", true, false, .{});
pub const positiveOnly: fn (
    arg0: c_int,
) Error!void = c_to_zig.translate("alpha_positive_only", true, false, .{});
const c_to_zig = api_translator.Translator(.{
    .c_import_ns = c,
    .substitutions = &.{
        .{ .old = c.alpha_struct, .new = Struct },
        .{ .old = [*c]const c.alpha_struct, .new = *const Struct },
    },
    .error_scheme = api_translator.BasicErrorScheme(Status, Error, .{
        .default_success_status = .ok,
        .default_failure_status = .failure,
        .default_error = Error.Unexpected,
    }),
});

test {
    inline for (comptime std.meta.declarations(@This())) |decl| {
        _ = @field(@This(), decl.name);
    }
}
