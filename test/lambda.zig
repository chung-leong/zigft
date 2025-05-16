const std = @import("std");
const api_translator = @import("api-translator.zig");
const c = @cImport({
    @cInclude("lambda.c");
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
pub const Struct = extern struct {
    number1: c_int,
    number2: c_int,
};
pub const StructPtr = *const Struct;

pub const acceptStruct1: fn (
    ptr: StructPtr,
) Error!void = c_to_zig.translate("lambda_accept_struct1", true, false, .{});

pub const isStruct1Null: fn () bool = c_to_zig.translate("lambda_is_struct1_null", false, false, .{});

pub const acceptStruct2: fn (
    ptr: ?*const Struct,
) Error!void = c_to_zig.translate("lambda_accept_struct2", true, false, .{ .@"0" = ?*const Struct });

pub const isStruct2Null: fn () bool = c_to_zig.translate("lambda_is_struct2_null", false, false, .{});

pub const Union = extern union {
    number1: c_int,
    number2: f32,
};

pub const acceptUnion: fn (
    ptr: ?Union,
) Error!void = c_to_zig.translate("lambda_accept_union", true, false, .{});

pub const isUnionNull: fn () bool = c_to_zig.translate("lambda_is_union_null", false, false, .{});

const c_to_zig = api_translator.Translator(.{
    .c_import_ns = c,
    .substitutions = &.{
        .{ .old = [*c]const c.lambda_union, .new = ?Union },
        .{ .old = c.lambda_struct_ptr, .new = StructPtr },
    },
    .error_scheme = api_translator.BasicErrorScheme(Status, Error, Error.Unexpected, .{}),
});

test {
    inline for (comptime std.meta.declarations(@This())) |decl| {
        _ = @field(@This(), decl.name);
    }
}
