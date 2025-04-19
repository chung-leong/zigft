const std = @import("std");
const api_translator = @import("api-translator.zig");
const c = @cImport({
    @cInclude("iota.c");
});

pub const setString: fn (
    s: []const u8,
) void = c_to_zig.translateMerge("iota_set_string", false, false, .{}, &.{
    .{ .ptr_index = 0, .len_index = 1 },
});

pub const getChar: fn (
    index: usize,
) u8 = c_to_zig.translate("iota_get_char", false, false, .{});

pub const getStringLen: fn () usize = c_to_zig.translate("iota_get_string_len", false, false, .{});

pub const setBytes: fn (
    s: []const u8,
) void = c_to_zig.translateMerge("iota_set_bytes", false, false, .{}, &.{
    .{ .ptr_index = 0, .len_index = 1 },
});

pub const getByte: fn (
    index: usize,
) c_int = c_to_zig.translate("iota_get_byte", false, false, .{});

pub const getByteLen: fn () usize = c_to_zig.translate("iota_get_byte_len", false, false, .{});

const c_to_zig = api_translator.Translator(.{
    .c_import_ns = c,
    .substitutions = &.{
        .{ .old = ?*const anyopaque, .new = *const anyopaque },
        .{ .old = [*c]const u8, .new = [*:0]const u8 },
    },
});

test {
    inline for (comptime std.meta.declarations(@This())) |decl| {
        _ = @field(@This(), decl.name);
    }
}
