const std = @import("std");
const page_allocator = std.heap.page_allocator;
const Chunk = @import("Chunk.zig").Chunk;
const OpCode = @import("Chunk.zig").OpCode;
const Value = @import("value.zig").Value;

pub fn main() !void {
    var c = Chunk.init(page_allocator);
    defer c.deinit();

    const constant = try c.addConstant(Value{ .double = 1.3 });
    try c.write(OpCode.constant, 1);
    try c.writeByte(constant, 1);
    try c.write(OpCode.constant, 2);
    try c.writeByte(constant, 2);

    try c.write(OpCode.@"return", 2);

    c.disassemble();
}
