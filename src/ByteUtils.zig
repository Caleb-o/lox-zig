const std = @import("std");
const print = std.debug.print;
const expect = std.testing.expect;
const eql = std.mem.eql;

pub fn unsignedIntToBytes(int: u32) [4]u8 {
    var arr = [_]u8{0} ** 4;
    arr[0] = @intCast(u8, (int >> 24) & 0xFF);
    arr[1] = @intCast(u8, (int >> 16) & 0xFF);
    arr[2] = @intCast(u8, (int >> 8) & 0xFF);
    arr[3] = @intCast(u8, int & 0xFF);

    return arr;
}

pub fn bytesToUnsignedInt(bytes: [4]u8) u32 {
    var int: u32 = 0;
    for (bytes) |byte| {
        int = (int << 8) | byte;
    }
    return int;
}

test "Int -> Bytes -> Int" {
    const value: u32 = 123_456;
    var bytes = unsignedIntToBytes(value);
    try expect(eql(u8, &bytes, &[4]u8{ 0, 1, 226, 64 }));
    var int = bytesToUnsignedInt(bytes);
    try expect(int == value);
}
