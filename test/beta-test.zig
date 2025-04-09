const std = @import("std");
const beta = @import("beta.zig");

const expectEqual = std.testing.expectEqual;
const expectError = std.testing.expectError;

test "acceptString" {
    try beta.acceptString("Hello");
    const c1 = try beta.getChar(1);
    try expectEqual('e', c1);
    const c2 = try beta.getChar(5);
    try expectEqual(0, c2);
}

test "acceptUnion" {
    const result = try beta.acceptUnion(&.{ .integer = 12345 });
    try expectEqual(12346, result.integer);
}

test "getMood" {
    try beta.getMood();
}

test "rejectArg" {
    const result = beta.rejectArg(123);
    try expectError(error.InvalidArg, result);
}
