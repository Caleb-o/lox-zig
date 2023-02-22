const std = @import("std");
const page_allocator = std.heap.page_allocator;
const Chunk = @import("Chunk.zig").Chunk;
const OpCode = @import("Chunk.zig").OpCode;
const Value = @import("value.zig").Value;
const VM = @import("VM.zig").VM;

pub fn main() !void {
    var vm = try VM.init(page_allocator);
    defer vm.deinit();

    var c = Chunk.init(page_allocator);
    defer c.deinit();

    const constant = try c.addConstant(Value{ .double = 1.3 });
    try c.write(OpCode.constant, 1);
    try c.writeByte(constant, 1);

    try c.write(OpCode.constant, 1);
    try c.writeByte(constant, 1);

    try c.write(OpCode.add, 1);

    const constant2 = try c.addConstant(Value{ .double = 3.0 });
    try c.write(OpCode.constant, 1);
    try c.writeByte(constant2, 1);
    try c.write(OpCode.divide, 1);

    try c.write(OpCode.@"return", 2);

    c.disassemble();

    _ = vm.setup(&c);
}
