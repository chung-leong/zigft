const std = @import("std");
const alpha = @import("alpha.zig");

const expectEqual = std.testing.expectEqual;
const expectError = std.testing.expectError;

test "acceptInt" {
    try alpha.acceptInt(123);
    const value = alpha.getInt();
    try expectEqual(123, value);
}

test "acceptEnum" {
    try alpha.acceptEnum(.happy);
    const value = alpha.getEnum();
    try expectEqual(.happy, value);
}

test "acceptStruct" {
    const input: alpha.Struct = .{
        .big_number = 12345,
        .small_number = 3,
    };
    const result = alpha.acceptStruct(&input);
    try expectEqual(alpha.Struct{ .big_number = 12346, .small_number = 4 }, result);
}

test "fail" {
    const result = alpha.fail();
    try expectError(alpha.Error.Failure, result);
}

test "failUnknown" {
    const result = alpha.failUnknown();
    try expectError(alpha.Error.Unknown, result);
}

test "positiveOnly" {
    const result = alpha.positiveOnly(-3);
    try expectError(alpha.Error.InvalidArg, result);
    try alpha.positiveOnly(3);
}
