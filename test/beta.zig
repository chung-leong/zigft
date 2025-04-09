const std = @import("std");
const api_translator = @import("api-translator.zig");
const c = @cImport({
    @cInclude("beta.c");
});

pub const Error = error{
    Failure,
    InvalidArg,
    Unexpected,
};
pub const Status = enum(c_int) {
    excellent = 2,
    good = 1,
    ok = 0,
    failure = -1,
    invalid_arg = -2,
    unexpected = -3,
    _,
};

pub const acceptString: fn (
    s: [*:0]const u8,
) Error!void = c_to_zig.translate("beta_accept_string", true, true, .{});

pub const getChar: fn (
    index: usize,
) Error!u8 = c_to_zig.translate("beta_get_char", true, true, .{});

pub const Union = extern union {
    integer: c_int,
    percentage: f64,
};

pub const acceptUnion: fn (
    arg0: *const Union,
) Error!Union = c_to_zig.translate("beta_accept_union", true, true, .{});

pub const Point = extern struct {
    x: c_int,
    y: c_int,
};

pub const getMood: fn () Error!void = c_to_zig.translate("beta_get_mood", true, true, .{});

pub const rejectArg: fn (
    arg0: c_int,
) Error!void = c_to_zig.translate("beta_reject_arg", true, true, .{});

const c_to_zig = api_translator.Translator(.{
    .c_import_ns = c,
    .substitutions = &.{
        .{ .old = [*c]const c.beta_union, .new = *const Union },
        .{ .old = [*c]const u8, .new = [*:0]const u8 },
        .{ .old = c.beta_union, .new = Union },
    },
    .error_scheme = api_translator.BasicErrorScheme(Status, Error, .{
        .default_success_status = .excellent,
        .default_failure_status = .failure,
        .default_error = Error.Unexpected,
    }),
});

test {
    inline for (comptime std.meta.declarations(@This())) |decl| {
        _ = @field(@This(), decl.name);
    }
}
