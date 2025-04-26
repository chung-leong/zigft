const std = @import("std");
const Napi = @import("napi.zig");

const Value = Napi.Value;
const CallbackInfo = Napi.Value;

comptime {
    Napi.createAddon(attachExports);
}

fn attachExports(env: *Napi, exports: Value) !void {
    inline for (comptime std.meta.declarations(@This())) |decl| {
        const func = @field(@This(), decl.name);
        const callback = try env.createCallback(decl.name, func, false, null);
        try env.setNamedProperty(exports, decl.name, callback);
    }
}

pub fn foo() void {
    std.debug.print("Hello world!\n", .{});
}

pub fn bar() !void {
    return error.IFoughtTheLawAndTheLawWon;
}

pub fn add(env: *Napi, arg1: Value, arg2: Value) !Value {
    const n1 = try env.getValueInt32(try env.coerceToNumber(arg1));
    const n2 = try env.getValueInt32(try env.coerceToNumber(arg2));
    return try env.createInt32(n1 + n2);
}

pub fn hello(env: *Napi, name: Value) !void {
    // coerce name to string
    const str = try env.coerceToString(name);
    // get UTF-8 string
    var buffer: [256]u8 = undefined;
    const len = try env.getValueStringUtf8(str, &buffer);
    const s = buffer[0..len];
    std.debug.print("Hello, {s}!\n", .{s});
}
