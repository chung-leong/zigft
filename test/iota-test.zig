const std = @import("std");
const iota = @import("iota.zig");

const expectEqual = std.testing.expectEqual;

test "setString" {
    iota.setString("Hello world");
    const len1 = iota.getStringLen();
    try expectEqual(11, len1);
    const c1 = iota.getChar(6);
    try expectEqual('w', c1);
    iota.setBytes("Hello world");
    const len2 = iota.getByteLen();
    try expectEqual(11, len2);
    const c2 = iota.getByte(7);
    try expectEqual('o', c2);
    iota.setBytes(null);
    const len3 = iota.getByteLen();
    try expectEqual(0, len3);
}
