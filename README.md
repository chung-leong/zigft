# Zigft

Zigft is a small library that lets you perform function transform in Zig. Consisting of just two 
files, it's designed to be used in source form. Simply download the file you need from this repo, 
place it in your `src` directory, and import it into your own code.

[`fn-transform.zig`](#fn-transformzig) provides the library's core functionality. 
[`fn-binding.zig`](#fn-bindingzig) meanwhile gives you the ability to bind variables to a function.

This project's code was developed original for [Zigar](https://github.com/chung-leong/zigar). 
Check it out of you haven't already learned of its existence.

## fn-transform.zig

`fn-transform.zig` provides a single function: 
[`spreadArgs()`](https://chung-leong.github.io/zigft/#zigft.fn-transform.spreadArgs). It takes a
function that accepts a tuple as the only argument and returns a new function where the tuple 
elements are spread across the argument list. For example, if the following function is the input:

```zig
fn hello(args: std.meta.Tuple(&.{ i8, i16, i32, i64 })) bool {
    // ...;
}
```

Then `spreadArgs(hello, null)` will return:

```zig
*const fn (i8, i16, i32, i64) bool
```

Because you have full control over the definition of the tuple at comptime, `spreadArgs()` 
basically lets you to generate any function you want. The only limitation is that its arguments 
cannot be `comptime` or `anytype`.

It's easier to see the function's purpose in action. Here're some usage scenarios:

#### Adding debug output to a function:

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

#### "Uninlining" an explicitly inline function:

```zig
const std = @import("std");
const fn_transform = @import("./fn-transform.zig");

fn Uninlined(comptime FT: type) type {
    const f = @typeInfo(FT).@"fn";
    if (f.calling_convention != .@"inline") return FT;
    return @Type(.{
        .@"fn" = .{
            .calling_convention = .auto,
            .is_generic = f.is_generic,
            .is_var_args = f.is_var_args,
            .return_type = f.return_type,
            .params = f.params,
        },
    });
}

fn uninline(func: anytype) Uninlined(@TypeOf(func)) {
    const FT = @TypeOf(func);
    const f = @typeInfo(FT).@"fn";
    if (f.calling_convention != .@"inline") return func;
    const ns = struct {
        inline fn call(args: std.meta.ArgsTuple(FT)) f.return_type.? {
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

#### Converting a function that returns an error code into one that returns an error union:

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

`fn-binding.zig` provides a [set of functions](https://chung-leong.github.io/zigft/#zigft.fn-binding)
related to function binding. [`bind()`](https://chung-leong.github.io/zigft/#zigft.fn-binding.bind)
and [`unbind()`](https://chung-leong.github.io/zigft/#zigft.fn-binding.unbind) are the pair you 
will most likely use.

The first argument to `bind()` can be either `fn (...)` or `*const fn (...)`. The second argument 
is a tuple containing arguments for the given function. The function returned by `bind()` depends 
on the tuple's content. If it provides a complete set of arguments, then the returned function 
will have an empty argument list. That is the case for the following example:

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

If you wish to bind to arguments in the middle of the argument list while leaving preceding 
ones unbound, you can do so with the help of explicit indices:

```zig
const std = @import("std");
const fn_binding = @import("./fn-binding.zig");

pub fn main() !void {
    const ns = struct {
        fn hello(a: i8, b: i16, c: i32, d: i64) void {
            std.debug.print("a = {d}, b = {d}, c = {d}, d = {d}\n", .{ a, b, c, d });
        }
    };
    const func1 = try fn_binding.bind(ns.hello, .{ .@"2" = 300 });
    defer fn_binding.unbind(func1);
    func1(1, 2, 4);
    const func2 = try fn_binding.bind(ns.hello, .{ .@"-2" = 301 });
    defer fn_binding.unbind(func2);
    func2(1, 2, 4);
}
```
```
a = 1, b = 2, c = 300, d = 4
a = 1, b = 2, c = 301, d = 4
```

Negative indices mean "from the end".

Binding to inline functions is possible:

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

Binding to inline functions with `comptime` or `anytype` arguments is impossible, however.

As you've seen already in the example involving 
[`std.debug.print()`](https://ziglang.org/documentation/0.14.0/std/#std.debug.print), binding to 
functions with `comptime` and `anytype` arguments is permitted as long as the resulting function 
will have no such arguments. 

In a `comptime` context, `bind()` would create a comptime binding. You would basically get a 
regular, not-dynamically-generated function:

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
```
Woof!
Meow!
???
```

Use [`define()`](https://chung-leong.github.io/zigft/#zigft.fn-binding.define) instead in this
scenario if you dislike the appearance of `catch unreachable`.

Function binding requires hardware-specific code. The CPU architectures listed here are currently 
supported: `x86_64`, `x86`, `aarch64`, `arm`, `riscv64`, `riscv32`, `powerpc64`, `powerpc64le`, 
`powerpc`.
