const std = @import("std");
const nu = @import("nu.zig");

const expectEqual = std.testing.expectEqual;
const expectError = std.testing.expectError;

test "hello" {
    const handle = try nu.get();
    try handle.hello();
}

test "world" {
    const handle = try nu.get();
    try handle.world(.{ .one = true });
    const error_result = handle.world(.{ .two = true });
    try expectError(error.Generic, error_result);
}

test "foo" {
    const handle = try nu.get();
    const result = handle.foo(.{ .one = true });
    try expectEqual(nu.Struct{
        .number = 1234,
        .options = .{
            .one = true,
        },
    }, result);
}
