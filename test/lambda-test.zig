const std = @import("std");
const lambda = @import("lambda.zig");

const expectEqual = std.testing.expectEqual;

test "acceptStruct1" {
    try expectEqual(true, lambda.isStruct1Null());
    try lambda.acceptStruct1(&.{ .number1 = 123, .number2 = 456 });
    try expectEqual(false, lambda.isStruct1Null());
}

test "acceptStruct2" {
    try expectEqual(true, lambda.isStruct2Null());
    try lambda.acceptStruct2(&.{ .number1 = 123, .number2 = 456 });
    try expectEqual(false, lambda.isStruct2Null());
    try lambda.acceptStruct2(null);
    try expectEqual(true, lambda.isStruct2Null());
}

test "acceptUnion" {
    try expectEqual(true, lambda.isUnionNull());
    try lambda.acceptUnion(.{ .number1 = 123 });
    try expectEqual(false, lambda.isUnionNull());
    try lambda.acceptUnion(null);
    try expectEqual(true, lambda.isUnionNull());
}
