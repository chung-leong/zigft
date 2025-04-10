const std = @import("std");
const zeta = @import("zeta.zig");

const expectEqual = std.testing.expectEqual;
const expectError = std.testing.expectError;

test "setCallback1" {
    const ns = struct {
        var called = false;

        fn call() callconv(.c) void {
            called = true;
        }
    };
    try zeta.setCallback1(&ns.call);
    try zeta.call1();
    try expectEqual(true, ns.called);
}

test "setCallback2" {
    const ns = struct {
        var called = false;
        var received: c_int = 0;

        fn call(arg: c_int) callconv(.c) zeta.Status {
            called = true;
            received = arg;
            return .ok;
        }
    };
    const result = zeta.call2(123);
    try expectError(error.Unexpected, result);
    try zeta.setCallback2(&ns.call);
    try zeta.call2(456);
    try expectEqual(true, ns.called);
    try expectEqual(456, ns.received);
}
