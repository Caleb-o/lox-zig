const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const debug = @import("debug.zig");
const valuem = @import("value.zig");
const Value = valuem.Value;
const ValueArray = valuem.ValueArray;

pub const OpCode = enum(u8) {
    Constant,
    ConstantShort,

    Nil,
    True,
    False,

    GetLocal,
    SetLocal,
    GetGlobal,
    DefineGlobal,
    SetGlobal,
    GetUpvalue,
    SetUpvalue,

    Pop,
    Equal,
    Greater,
    Less,

    Add,
    Subtract,
    Multiply,
    Divide,
    Not,
    Negate,
    Print,

    Closure,
    Call,
    Loop,
    Jump,
    JumpIfFalse,
    Return,
};

// Fields
allocator: Allocator,
code: ArrayList(u8),
constant_pool: ValueArray,
// NOTE: This doesn't work with multiple files
//       We will need a new representation when that comes
line_info: ArrayList(u32),

// Methods / Functions
pub const Chunk = @This();

pub fn init(allocator: Allocator) Chunk {
    return Chunk{
        .allocator = allocator,
        .code = .empty,
        .constant_pool = ValueArray.init(allocator),
        .line_info = .empty,
    };
}

pub inline fn deinit(self: *Chunk) void {
    self.constant_pool.deinit();
    self.code.deinit(self.allocator);
    self.line_info.deinit(self.allocator);
}

pub fn writeByte(self: *Chunk, opcode: u8, line: u32) !void {
    try self.code.append(self.allocator, opcode);
    try self.addOrIncLineNumber(line);
}

pub fn write(self: *Chunk, opcode: OpCode, line: u32) !void {
    try self.code.append(self.allocator, @intFromEnum(opcode));
    try self.addOrIncLineNumber(line);
}

pub inline fn disassemble(self: *Chunk) void {
    debug.disassembleChunk(self, "Test");
}

pub inline fn addConstant(self: *Chunk, value: Value) !u8 {
    try self.constant_pool.write(value);
    return @intCast(self.constant_pool.values.items.len - 1);
}

pub fn findOpcodeLine(self: *Chunk, offset: usize) usize {
    var sum: usize = 0;

    for (self.line_info.items, 0..) |linec, line| {
        sum += linec;

        if (offset < sum) {
            return line + 1;
        }
    }

    return 0;
}

fn addOrIncLineNumber(self: *Chunk, line: u32) !void {
    var current = self.line_info.items.len;

    // Use run-length encoding
    if (current < line) {
        while (current < line) : (current += 1) {
            try self.line_info.append(self.allocator, 0);
        }
    }

    // Increment
    self.line_info.items[line - 1] += 1;
}
