const std = @import("std");

const fn_binding = @import("zigft/fn-binding.zig");

var gpa: std.heap.DebugAllocator(.{}) = .init;

export const malloc = fn_binding.defineWithCallConv(std.mem.Allocator.rawAlloc, .{
    .@"0" = gpa.allocator(),
    .@"2" = .@"16", // alignment
    .@"3" = 0, // ret_addr
}, .c);

pub fn main() !void {}
