const std = @import("std");
const kappa = @import("kappa.zig");

const expectEqual = std.testing.expectEqual;

test "doStuff" {
    try kappa.doStuff();
}

test "getLastStatus" {
    const status = kappa.getLastStatus();
    try expectEqual(kappa.Status.failure, status);
}
