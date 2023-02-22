const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Chunk = @import("Chunk.zig").Chunk;
const OpCode = @import("Chunk.zig").OpCode;
const Value = @import("value.zig").Value;
const print = std.debug.print;

pub const Compiler = @This();

// Fields

pub fn compile(self: *Compiler) void {
    _ = self;
}
