const std = @import("std");
const api_translator = @import("api-translator.zig");
const c = @cImport({
    @cInclude("eta.c");
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
pub const Hello = enum(c_uint) {
    apple,
    banana,
    chicken,
    _,
};
pub const Struct = extern struct {
    number1: c_int,
    number2: c_int,
};
pub const Callback1 = *const fn () callconv(.c) void;
pub const Callback2 = *const fn (
    *const Struct,
    Hello,
) callconv(.c) Status;
pub const CallTable = extern struct {
    number1: usize,
    number2: usize,
    callback1: Callback1,
    callback2: Callback2,
};

pub const setCallTable: fn (
    t: CallTable,
) Error!void = c_to_zig.translate("eta_set_call_table", true, false, .{});

pub const call1: fn () Error!void = c_to_zig.translate("eta_call1", true, false, .{});

pub const call2: fn (
    arg0: Struct,
    arg1: Hello,
) Error!void = c_to_zig.translate("eta_call2", true, false, .{ .@"1" = Hello });

const c_to_zig = api_translator.Translator(.{
    .c_import_ns = c,
    .substitutions = &.{
        .{ .old = [*c]const c.eta_call_table, .new = CallTable },
        .{ .old = [*c]const c.eta_struct, .new = *const Struct },
    },
    .error_scheme = api_translator.BasicErrorScheme(Status, Error, Error.Unexpected),
});

test {
    inline for (comptime std.meta.declarations(@This())) |decl| {
        _ = @field(@This(), decl.name);
    }
}
