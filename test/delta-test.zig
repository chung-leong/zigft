const std = @import("std");
const delta = @import("delta.zig");

const expectEqual = std.testing.expectEqual;
const expectError = std.testing.expectError;

test "acceptInt" {
    try delta.acceptInt(123);
}

test "fail" {
    const result = delta.fail();
    try expectError(error.Unexpected, result);
}
