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
    const f = @typeInfo(FT).@"fn";
    var param_types: [f.params.len]type = undefined;
    var param_attrs: [f.params.len]std.builtin.Type.Fn.Param.Attributes = undefined;
    inline for (f.params, 0..) |param, i| {
        param_types[i] = param.type.?;
        param_attrs[i] = .{ .@"noalias" = param.is_noalias };
    }
    const RT = NewErrorSet!void;
    const NFT = @Fn(&param_types, &param_attrs, RT, .{
        .varargs = f.is_var_args,
    });
    return NFT;
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
