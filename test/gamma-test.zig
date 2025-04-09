const std = @import("std");
const gamma = @import("gamma.zig");

const expectEqual = std.testing.expectEqual;
const expectError = std.testing.expectError;

test "acceptInt" {
    try gamma.acceptInt(123);
    const result = gamma.getInt();
    try expectEqual(123, result);
}

test "fail" {
    const result = gamma.fail();
    try expectError(error.Unexpected, result);
}
