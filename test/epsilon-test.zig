const std = @import("std");
const epsilon = @import("epsilon.zig");

test "acceptInt" {
    epsilon.acceptInt(123);
}
