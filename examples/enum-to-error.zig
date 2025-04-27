const std = @import("std");
const fn_transform = @import("zigft/fn-transform.zig");

const OriginalErrorEnum = enum(c_int) {
    OK,
    APPLE_IS_ROTTING,
    BANANA_STINKS,
    CANTALOUPE_EXPLODED,
};

fn originalFn() callconv(.c) OriginalErrorEnum {
    return .CANTALOUPE_EXPLODED;
}

const NewErrorSet = error{
    AppleIsRotting,
    BananaStink,
    CantaloupeExploded,
};

fn Translated(comptime FT: type) type {
    return @Type(.{
        .@"fn" = .{
            .calling_convention = .auto,
            .is_generic = false,
            .is_var_args = false,
            .return_type = NewErrorSet!void,
            .params = @typeInfo(FT).@"fn".params,
        },
    });
}

fn translate(comptime func: anytype) Translated(@TypeOf(func)) {
    const error_list = init: {
        const es = @typeInfo(NewErrorSet).error_set.?;
        var list: [es.len]NewErrorSet = undefined;
        inline for (es, 0..) |e, index| {
            list[index] = @field(NewErrorSet, e.name);
        }
        break :init list;
    };
    const FT = @TypeOf(func);
    const TFT = Translated(FT);
    const ns = struct {
        inline fn call(args: std.meta.ArgsTuple(TFT)) NewErrorSet!void {
            const result = @call(.auto, func, args);
            if (result != .OK) {
                const index: usize = @intCast(@intFromEnum(result) - 1);
                return error_list[index];
            }
        }
    };
    return fn_transform.spreadArgs(ns.call, .auto);
}

pub fn main() !void {
    const func = translate(originalFn);
    try func();
}
