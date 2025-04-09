const std = @import("std");
const beta = @import("beta-with-status.zig");

const expectEqual = std.testing.expectEqual;
const expectError = std.testing.expectError;

test "acceptString" {
    const status = try beta.acceptString("Hello");
    try expectEqual(.excellent, status);
    const c1, const s1 = try beta.getChar(1);
    try expectEqual(.ok, s1);
    try expectEqual('e', c1);
    const c2, _ = try beta.getChar(5);
    try expectEqual(0, c2);
}

test "acceptUnion" {
    const input: beta.Union = .{ .integer = 12345 };
    const result, const status = try beta.acceptUnion(&input);
    try expectEqual(12346, result.integer);
    try expectEqual(.ok, status);
}

test "getMood" {
    const status = try beta.getMood();
    try expectEqual(.good, status);
}

test "rejectArg" {
    const result = beta.rejectArg(123);
    try expectError(error.InvalidArg, result);
}
