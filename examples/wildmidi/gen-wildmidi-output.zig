const std = @import("std");
const api_translator = @import("zigft/api-translator.zig");
const c = @cImport({
    @cInclude("wm_error.h");
    @cInclude("wildmidi_lib.h");
});

pub const Error = error{
    Mem,
    Stat,
    Load,
    Open,
    Read,
    Invalid,
    Corupt,
    NotInit,
    InvalidArg,
    AlrInit,
    NotMidi,
    Longfil,
    NotHmp,
    NotHmi,
    Convert,
    NotMus,
    NotXmi,
    Max,
    Unexpected,
};
pub const ErrorEnum = enum(c_int) {
    none,
    mem,
    stat,
    load,
    open,
    read,
    invalid,
    corupt,
    not_init,
    invalid_arg,
    alr_init,
    not_midi,
    longfil,
    not_hmp,
    not_hmi,
    convert,
    not_mus,
    not_xmi,
    max,
    _,
};
pub const _WM_Global_ErrorS = &c._WM_Global_ErrorS;
pub const _WM_Global_ErrorI = &c._WM_Global_ErrorI;

pub const globalErrorInternal: fn (
    func: [*:0]const u8,
    lne: c_int,
    wmerno: c_int,
    wmfor: [*:0]const u8,
    @"error": c_int,
) void = c_to_zig.translate("_WM_GLOBAL_ERROR_INTERNAL", false, false, .{});

pub const errorNew: fn (
    wmfmt: [*:0]const u8,
) void = c_to_zig.translate("_WM_ERROR_NEW", false, false, .{});

pub const debugMsg: fn (
    wmfmt: [*:0]const u8,
) void = c_to_zig.translate("_WM_DEBUG_MSG", false, false, .{});

pub const VIOAllocate = *const fn (
    [*:0]const u8,
    *u32,
) callconv(.c) *anyopaque;
pub const VIOFree = *const fn (
    *anyopaque,
) callconv(.c) void;

pub const getString: fn (
    info: u16,
) [*:0]const u8 = c_to_zig.translate("WildMidi_GetString", false, false, .{});

pub const getVersion: fn () c_long = c_to_zig.translate("WildMidi_GetVersion", false, false, .{});

pub const init: fn (
    config_file: [*:0]const u8,
    rate: u16,
    mixer_options: MixerOptions,
) Error!void = c_to_zig.translate("WildMidi_Init", true, false, .{ .@"2" = MixerOptions });

pub const initVIO: fn (
    callbacks: *Vio,
    config_file: [*:0]const u8,
    rate: u16,
    mixer_options: MixerOptions,
) Error!void = c_to_zig.translate("WildMidi_InitVIO", true, false, .{ .@"3" = MixerOptions });

pub const masterVolume: fn (
    master_volume: u8,
) Error!void = c_to_zig.translate("WildMidi_MasterVolume", true, false, .{});

pub const open: fn (
    midifile: [*:0]const u8,
) *@This() = c_to_zig.translate("WildMidi_Open", false, false, .{});

pub const openBuffer: fn (
    midibuffer: [*:0]const u8,
    size: u32,
) *@This() = c_to_zig.translate("WildMidi_OpenBuffer", false, false, .{});

pub const getMidiOutput: fn (
    handle: *@This(),
) Error!std.meta.Tuple(&.{ *i8, u32 }) = c_to_zig.translate("WildMidi_GetMidiOutput", true, false, .{});

pub const getOutput: fn (
    handle: *@This(),
    buffer: *i8,
    size: u32,
) Error!void = c_to_zig.translate("WildMidi_GetOutput", true, false, .{});

pub const setOption: fn (
    handle: *@This(),
    options: u16,
    setting: u16,
) Error!void = c_to_zig.translate("WildMidi_SetOption", true, false, .{});

pub const setCvtOption: fn (
    tag: u16,
    setting: u16,
) Error!void = c_to_zig.translate("WildMidi_SetCvtOption", true, false, .{});

pub const convertToMidi: fn (
    file: [*:0]const u8,
) Error!std.meta.Tuple(&.{ [*:0]u8, u32 }) = c_to_zig.translate("WildMidi_ConvertToMidi", true, false, .{});

pub const convertBufferToMidi: fn (
    in: [*:0]const u8,
    insize: u32,
) Error!std.meta.Tuple(&.{ [*:0]u8, u32 }) = c_to_zig.translate("WildMidi_ConvertBufferToMidi", true, false, .{});

pub const getInfo: fn (
    handle: *@This(),
) *Info = c_to_zig.translate("WildMidi_GetInfo", false, false, .{});

pub const fastSeek: fn (
    handle: *@This(),
) Error!c_ulong = c_to_zig.translate("WildMidi_FastSeek", true, false, .{});

pub const songSeek: fn (
    handle: *@This(),
    nextsong: i8,
) Error!void = c_to_zig.translate("WildMidi_SongSeek", true, false, .{});

pub const close: fn (
    handle: *@This(),
) Error!void = c_to_zig.translate("WildMidi_Close", true, false, .{});

pub const shutdown: fn () Error!void = c_to_zig.translate("WildMidi_Shutdown", true, false, .{});

pub const getLyric: fn (
    handle: *@This(),
) [*:0]u8 = c_to_zig.translate("WildMidi_GetLyric", false, false, .{});

pub const getError: fn () [*:0]u8 = c_to_zig.translate("WildMidi_GetError", false, false, .{});

pub const clearError: fn () void = c_to_zig.translate("WildMidi_ClearError", false, false, .{});

pub const MixerOptions = packed struct(u16) {
    log_volume: bool = false,
    enhanced_resampling: bool = false,
    reverb: bool = false,
    loop: bool = false,
    _: u8 = 0,
    saveastype0: bool = false,
    roundtempo: bool = false,
    stripsilence: bool = false,
    textaslyric: bool = false,
    __: std.meta.Int(.unsigned, @bitSizeOf(u16) - 16) = 0,
};
pub const Info = extern struct {
    copyright: [*:0]u8,
    current_sample: u32,
    approx_total_samples: u32,
    mixer_options: u16,
    total_midi_time: u32,
};
pub const Vio = extern struct {
    allocate_file: VIOAllocate,
    free_file: VIOFree,
};
const c_to_zig = api_translator.Translator(.{
    .c_import_ns = c,
    .substitutions = &.{
        .{ .old = ?*c.midi, .new = *@This() },
        .{ .old = [*c]c.struct__WM_Info, .new = *Info },
        .{ .old = [*c]c.struct__WM_VIO, .new = *Vio },
        .{ .old = [*c]const u8, .new = [*:0]const u8 },
        .{ .old = [*c]i8, .new = *i8 },
        .{ .old = [*c]u32, .new = *u32 },
        .{ .old = [*c]u8, .new = [*:0]u8 },
    },
    .error_scheme = api_translator.BasicErrorScheme(ErrorEnum, Error, Error.Unexpected),
});

test {
    inline for (comptime std.meta.declarations(@This())) |decl| {
        _ = @field(@This(), decl.name);
    }
}
