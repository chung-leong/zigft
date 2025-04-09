const std = @import("std");
const alpha = @import("alpha-by-value.zig");

const expectEqual = std.testing.expectEqual;

test "acceptStruct" {
    const result = alpha.acceptStruct(.{
        .big_number = 12345,
        .small_number = 3,
    });
    try expectEqual(alpha.Struct{ .big_number = 12346, .small_number = 4 }, result);
}
