const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const debug = @import("debug.zig");
const valuem = @import("value.zig");
const Value = valuem.Value;
const ValueArray = valuem.ValueArray;

pub const OpCode = enum(u8) {
    constant,
    // constant_long, TODO
    @"return",
};

// Fields
code: ArrayList(u8),
constant_pool: ValueArray,
// NOTE: This doesn't work with multiple files
//       We will need a new representation when that comes
line_info: ArrayList(u32),

// Methods / Functions
pub const Chunk = @This();

pub fn init(allocator: Allocator) Chunk {
    return Chunk{
        .code = ArrayList(u8).init(allocator),
        .constant_pool = ValueArray.init(allocator),
        .line_info = ArrayList(u32).init(allocator),
    };
}

pub inline fn deinit(self: *Chunk) void {
    self.constant_pool.deinit();
    self.code.deinit();
    self.line_info.deinit();
}

pub fn writeByte(self: *Chunk, opcode: u8, line: u32) !void {
    try self.code.append(opcode);
    try self.addOrIncLineNumber(line);
}

pub fn write(self: *Chunk, opcode: OpCode, line: u32) !void {
    try self.code.append(@enumToInt(opcode));
    try self.addOrIncLineNumber(line);
}

pub inline fn disassemble(self: *Chunk) void {
    debug.disassembleChunk(self, "Test");
}

pub inline fn addConstant(self: *Chunk, value: Value) !u8 {
    try self.constant_pool.write(value);
    return @intCast(u8, self.constant_pool.values.items.len - 1);
}

pub fn findOpcodeLine(self: *Chunk, offset: u32) u32 {
    var sum: u32 = 0;

    for (self.line_info.items) |linec, line| {
        sum += linec;

        if (offset < sum) {
            return @intCast(u32, line + 1);
        }
    }

    return 0;
}

fn addOrIncLineNumber(self: *Chunk, line: u32) !void {
    var current = self.line_info.items.len;

    // Use run-length encoding
    if (current < line) {
        while (current < line) : (current += 1) {
            try self.line_info.append(0);
        }
    }

    // Increment
    self.line_info.items[line - 1] += 1;
}
