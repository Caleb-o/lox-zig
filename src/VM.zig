const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Chunk = @import("Chunk.zig").Chunk;
const OpCode = @import("Chunk.zig").OpCode;
const Value = @import("value.zig").Value;
const print = std.debug.print;

pub const InterpretResult = enum(u8) {
    ok,
    compilerError,
    runtimeError,
};

// Fields
chunk: ?*Chunk,
ip: usize,
stack: ArrayList(Value),

// Methods
pub const VM = @This();

pub fn init(allocator: Allocator) !VM {
    return VM{
        .chunk = null,
        .ip = 0,
        .stack = try ArrayList(Value).initCapacity(allocator, 256),
    };
}

pub fn deinit(self: *VM) void {
    self.stack.deinit();
}

pub fn setup(self: *VM, chunk: *Chunk) InterpretResult {
    self.chunk = chunk;
    self.ip = 0;
    return self.run();
}

fn resetStack(self: *VM) void {
    self.stack.clearAndFree();
}

inline fn push(self: *VM, value: Value) void {
    self.stack.append(value) catch {};
}

inline fn pop(self: *VM) Value {
    return self.stack.pop();
}

fn run(self: *VM) InterpretResult {
    while (true) {
        const instruction = self.readByte();
        switch (@intToEnum(OpCode, instruction)) {
            OpCode.@"return" => {
                var c = self.pop();
                c.print();
                print("\n", .{});
                return InterpretResult.ok;
            },
            OpCode.constant => {
                self.push(self.readConstant());
            },
            OpCode.add => self.binaryOp('+'),
            OpCode.subtract => self.binaryOp('-'),
            OpCode.multiply => self.binaryOp('*'),
            OpCode.divide => self.binaryOp('/'),
            OpCode.negate => {
                var v = self.pop();
                switch (v) {
                    .double => |n| self.push(Value{ .double = -n }),
                }
            },
        }
    }
    return InterpretResult.ok;
}

// Helper functions
fn binaryOp(self: *VM, op: u8) void {
    var rhs = self.pop();
    var lhs = self.pop();

    switch (op) {
        '+' => self.push(Value{ .double = lhs.double + rhs.double }),
        '-' => self.push(Value{ .double = lhs.double - rhs.double }),
        '*' => self.push(Value{ .double = lhs.double * rhs.double }),
        '/' => self.push(Value{ .double = lhs.double / rhs.double }),
        else => {},
    }
}

inline fn readByte(self: *VM) u8 {
    defer self.ip += 1;
    return self.chunk.?.code.items[self.ip];
}

inline fn readConstant(self: *VM) Value {
    return self.chunk.?.constant_pool.values.items[self.readByte()];
}
