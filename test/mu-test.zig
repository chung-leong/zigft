const std = @import("std");
const mu = @import("mu.zig");

const expectEqual = std.testing.expectEqual;
const expectError = std.testing.expectError;

test "getVoidPtr" {
    const result = mu.getVoidPtr(false);
    try expectError(error.NullPointer, result);
    _ = try mu.getVoidPtr(true);
}

test "getVoidPtrNoError" {
    _ = mu.getVoidPtrNoError();
}

test "getIntPtr" {
    const result1 = mu.getIntPtr(false);
    try expectError(error.NullPointer, result1);
    const result2 = try mu.getIntPtr(true);
    try expectEqual(1234, result2.*);
}

test "getHandle" {
    const result1 = mu.getHandle(false);
    try expectError(error.InvalidHandle, result1);
    const result2 = try mu.getHandle(true);
    try expectEqual(1234, result2);
}
