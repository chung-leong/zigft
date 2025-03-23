# zigft
Zig function transform library

## fn-transform.zig

Adding debug output to a function:
```zig
const std = @import("std");
const fn_transform = @import("./fn-transform.zig");

fn attachDebugOutput(comptime func: anytype, comptime name: []const u8) @TypeOf(func) {
    const FT = @TypeOf(func);
    const fn_info = @typeInfo(FT).@"fn";
    const ns = struct {
        inline fn call(args: std.meta.ArgsTuple(FT)) fn_info.return_type.? {
            std.debug.print("{s}: {any}\n", .{ name, args });
            return @call(.auto, func, args);
        }
    };
    return fn_transform.spreadArgs(ns.call, fn_info.calling_convention);
}

pub fn main() void {
    const ns = struct {
        fn hello(a: i32, b: i32) void {
            std.debug.print("sum = {d}\n", .{a + b});
        }
    };
    const func = attachDebugOutput(ns.hello, "hello");
    func(123, 456);
}
```
```
hello: { 123, 456 }
sum = 579
```

Uninlining an inline function:
```zig
const std = @import("std");
const fn_transform = @import("./fn-transform.zig");

pub fn Uninlined(comptime FT: type) type {
    return switch (@typeInfo(FT)) {
        .@"fn" => |f| @Type(.{
            .@"fn" = .{
                .calling_convention = switch (f.calling_convention) {
                    .@"inline" => .auto,
                    else => |cc| cc,
                },
                .is_generic = f.is_generic,
                .is_var_args = f.is_var_args,
                .return_type = f.return_type,
                .params = f.params,
            },
        }),
        else => @compileError("Not a function"),
    };
}

fn uninline(comptime func: anytype) Uninlined(@TypeOf(func)) {
    const FT = @TypeOf(func);
    const ns = struct {
        inline fn call(args: std.meta.ArgsTuple(FT)) @typeInfo(FT).@"fn".return_type.? {
            return @call(.auto, func, args);
        }
    };
    return fn_transform.spreadArgs(ns.call, .auto);
}

pub fn main() void {
    const ns = struct {
        inline fn hello(a: i32, b: i32) void {
            std.debug.print("sum = {d}\n", .{a + b});
        }
    };
    const func = uninline(ns.hello);
    std.debug.print("fn address = {x}\n", .{@intFromPtr(&func)});
}
```
```
fn address = 10deb00
```

Converting a function that returns an error code into one that returns an error union:
```zig
const std = @import("std");
const fn_transform = @import("./fn-transform.zig");

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
```
```
error: CantaloupeExploded
/home/cleong/zigft/fn-transform.zig:97:13: 0x10de01e in call0 (example-enum-to-error)
            return func(.{});
            ^
/home/cleong/zigft/example-enum-to-error.zig:58:5: 0x10ddf53 in main (example-enum-to-error)
    try func();
```

## fn-binding.zig

```zig
const std = @import("std");
const fn_binding = @import("./fn-binding.zig");

pub fn main() !void {
    var funcs: [5]*const fn () void = undefined;
    for (&funcs, 0..) |*ptr, index|
        ptr.* = try fn_binding.bind(std.debug.print, .{ "hello: {d}\n", .{index + 1} });
    defer for (funcs) |f| fn_binding.unbind(f);
    for (funcs) |f| f();
}
```
```
hello: 1
hello: 2
hello: 3
hello: 4
hello: 5
```


```zig
const std = @import("std");
const fn_binding = @import("./fn-binding.zig");

pub fn main() !void {
    const ns = struct {
        inline fn hello(a: i32, b: i32) void {
            std.debug.print("sum = {d}\n", .{a + b});
        }
    };
    const func = try fn_binding.bind(ns.hello, .{ .@"-1" = 123 });
    func(3);
}
```
```
sum = 126
```


```zig
const std = @import("std");
const fn_binding = @import("./fn-binding.zig");

pub fn main() !void {
    const ns = struct {
        const dog = fn_binding.bind(std.debug.print, .{ "Woof!\n", .{} }) catch unreachable;
        const cat = fn_binding.bind(std.debug.print, .{ "Meow!\n", .{} }) catch unreachable;
        const fox = fn_binding.define(std.debug.print, .{ "???\n", .{} });
    };
    ns.dog();
    ns.cat();
    ns.fox();
}
```
Woof!
Meow!
???
```