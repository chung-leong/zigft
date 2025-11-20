const std = @import("std");

const fn_binding = @import("zigft/fn-binding.zig");

fn allocateFrom(allocator: *const std.mem.Allocator, len: usize) callconv(.c) [*c]u8 {
    const bytes = allocator.alignedAlloc(u8, .@"8", len) catch return null;
    return bytes.ptr;
}

pub fn main() !void {
    var buffer: [1024]u8 = undefined;
    var fba: std.heap.FixedBufferAllocator = .init(&buffer);
    const allocator = fba.allocator();
    const alloc_fn = try fn_binding.bind(allocateFrom, .{&allocator});
    defer fn_binding.unbind(alloc_fn);
    std.debug.print("Buffer address:      {X}\n", .{@intFromPtr(&buffer)});
    foo(0x100, alloc_fn);
}

fn foo(len: usize, alloc: *const fn (len: usize) callconv(.c) [*c]u8) void {
    for (0..10) |_| {
        const p = alloc(len);
        if (p != null) {
            std.debug.print("Allocated {d} bytes: {X}\n", .{ len, @intFromPtr(p) });
        } else {
            std.debug.print("Couldn't allocated memory\n", .{});
            break;
        }
    }
}
