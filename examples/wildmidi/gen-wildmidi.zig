const std = @import("std");
const api_translator = @import("zigft/api-translator.zig");

pub fn main() !void {
    // create instance of generator
    var gpa: std.heap.DebugAllocator(.{}) = .init;
    var generator: *api_translator.CodeGenerator(.{
        .include_paths = &.{"wildmidi/include"},
        .header_paths = &.{ "wm_error.h", "wildmidi_lib.h" },
        .zigft_path = "zigft/",
        .c_error_type = "int",
        .c_root_struct = "midi",
        .error_enum = "ErrorEnum",
        .filter_fn = filter,
        .param_override_fn = overrideParam,
        .type_name_fn = getTypeName,
        .fn_name_fn = getFnName,
        .enum_name_fn = getEnumName,
        .error_name_fn = getErrorName,
        .const_is_enum_item_fn = isEnumItem,
    }) = try .init(gpa.allocator());
    defer generator.deinit();
    // analyze the headers
    try generator.analyze();
    // save translated code to file
    const path = try std.fs.path.resolve(generator.allocator, &.{
        generator.cwd,
        "gen-wildmidi-output.zig",
    });
    var file = try std.fs.createFileAbsolute(path, .{});
    try generator.print(file.writer());
    file.close();
}

const camelize = api_translator.camelize;
const snakify = api_translator.snakify;
const prefixes = .{
    "WildMidi_",
    "_WM",
};
const enum_types = .{
    .{
        .prefix = "WM_ERR_",
        .type = api_translator.EnumInfo{ .name = "ErrorEnum", .tag_type = "c_int" },
    },
    .{
        .prefix = "WM_MO_",
        .type = api_translator.EnumInfo{ .name = "MixerOptions", .tag_type = "u16", .is_packed_struct = true },
    },
};
const type_overrides = .{
    .mixer_options = "MixerOptions",
};

fn getNameOffset(name: []const u8) usize {
    return inline for (prefixes) |prefix| {
        if (std.mem.startsWith(u8, name, prefix)) break prefix.len;
    } else 0;
}

fn filter(name: []const u8) bool {
    return getNameOffset(name) > 0;
}

fn overrideParam(_: []const u8, param_name: ?[]const u8, _: usize, _: []const u8) ?[]const u8 {
    return inline for (std.meta.fields(@TypeOf(type_overrides))) |field| {
        if (param_name) |name| {
            if (std.mem.eql(u8, name, field.name)) break @field(type_overrides, field.name);
        }
    } else null;
}

fn isEnumItem(name: []const u8) ?api_translator.EnumInfo {
    return inline for (enum_types) |t| {
        if (std.mem.startsWith(u8, name, t.prefix)) break comptime t.type;
    } else null;
}

fn getFnName(allocator: std.mem.Allocator, name: []const u8) ![]const u8 {
    return camelize(allocator, name, getNameOffset(name), false);
}

fn getTypeName(allocator: std.mem.Allocator, name: []const u8) ![]const u8 {
    return camelize(allocator, name, getNameOffset(name), true);
}

fn getEnumName(allocator: std.mem.Allocator, name: []const u8) ![]const u8 {
    return inline for (enum_types) |t| {
        if (std.mem.startsWith(u8, name, t.prefix))
            break snakify(allocator, name, t.prefix.len);
    } else name;
}

fn getErrorName(allocator: std.mem.Allocator, name: []const u8) ![]const u8 {
    return camelize(allocator, name, 0, true);
}
