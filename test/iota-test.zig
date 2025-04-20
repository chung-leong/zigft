const std = @import("std");
const iota = @import("iota.zig");

const expectEqual = std.testing.expectEqual;

test "setString" {
    iota.setString("Hello world");
    const len = iota.getStringLen();
    try expectEqual(11, len);
    const c = iota.getChar(6);
    try expectEqual('w', c);
}

test "setBytes" {
    iota.setBytes("Hello world");
    const len1 = iota.getByteLen();
    try expectEqual(11, len1);
    const c = iota.getByte(7);
    try expectEqual('o', c);
    iota.setBytes(null);
    const len2 = iota.getByteLen();
    try expectEqual(0, len2);
}
