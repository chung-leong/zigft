const std = @import("std");
const theta = @import("theta.zig");

const expectEqual = std.testing.expectEqual;
const expectError = std.testing.expectError;

test "isSleepy" {
    const result1 = theta.isSleepy(.{ .happy = true });
    try expectError(error.WrongDwarf, result1);
    const result2 = theta.isSleepy(.{ .happy = true, .sleepy = true });
    try expectEqual({}, result2);
}

test "setDwarves" {
    try theta.setDwarves(.{ .sneezy = true, .dopey = true, .bashful = false });
    const ns = struct {
        var received: theta.Flags = .{};

        fn call(arg: theta.Flags) callconv(.c) void {
            received = arg;
        }
    };
    theta.invoke(&ns.call);
    try expectEqual(theta.Flags{ .sneezy = true, .dopey = true }, ns.received);
}
