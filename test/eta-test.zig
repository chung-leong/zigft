const std = @import("std");
const eta = @import("eta.zig");

const expectEqual = std.testing.expectEqual;
const expectError = std.testing.expectError;

test "setCallTable" {
    const ns = struct {
        var called1 = false;
        var called2 = false;
        var received0: ?eta.Struct = null;
        var received1: ?eta.Hello = null;

        fn call1() callconv(.c) void {
            called1 = true;
        }

        fn call2(arg0: *const eta.Struct, arg1: eta.Hello) callconv(.c) eta.Status {
            received0 = arg0.*;
            received1 = arg1;
            return .ok;
        }
    };
    try eta.setCallTable(.{
        .number1 = 123,
        .number2 = 456,
        .callback1 = &ns.call1,
        .callback2 = &ns.call2,
    });
    try eta.call1();
    try expectEqual(true, ns.called1);
    try eta.call2(.{ .number1 = 777, .number2 = 888 }, .chicken);
    try expectEqual(eta.Struct{ .number1 = 777, .number2 = 888 }, ns.received0);
    try expectEqual(.chicken, ns.received1);
}
