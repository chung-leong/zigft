const std = @import("std");
const api_translator = @import("api-translator.zig");
const c = @cImport({
    @cInclude("epsilon.c");
});

pub const acceptInt: fn (
    arg0: c_int,
) void = c_to_zig.translate("epsilon_accept_int", false, false, .{});

const c_to_zig = api_translator.Translator(.{
    .c_import_ns = c,
});

test {
    inline for (comptime std.meta.declarations(@This())) |decl| {
        _ = @field(@This(), decl.name);
    }
}
