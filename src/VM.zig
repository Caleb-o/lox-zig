const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Chunk = @import("Chunk.zig").Chunk;
const OpCode = @import("Chunk.zig").OpCode;
const Value = @import("value.zig").Value;
const Compiler = @import("Compiler.zig");
const print = std.debug.print;
const debug = @import("debug.zig");

pub const Self = @This();

pub const InterpretResult = enum(u8) {
    ok,
    compilerError,
    runtimeError,
};

// Fields
allocator: Allocator,
chunk: ?*Chunk,
ip: usize,
stack: ArrayList(Value),

// Methods

pub fn init(allocator: Allocator) !Self {
    return .{
        .allocator = allocator,
        .chunk = null,
        .ip = 0,
        .stack = try ArrayList(Value).initCapacity(allocator, 256),
    };
}

pub fn deinit(self: *Self) void {
    self.stack.deinit();
}

pub fn setup_and_go(self: *Self, source: []const u8) InterpretResult {
    var chunk = Chunk.init(self.allocator);
    defer chunk.deinit();

    var compiler = Compiler.create();
    if (!compiler.compile(&chunk, source)) {
        return .compilerError;
    }

    self.chunk = &chunk;
    self.ip = 0;

    return self.run();
}

inline fn resetStack(self: *Self) void {
    self.stack.clearAndFree();
}

fn runtimeError(self: *Self, msg: []const u8) void {
    const line = self.chunk.?.findOpcodeLine(self.ip);
    std.debug.print("Error: {s} [line {d}]\n", .{ msg, line });
}

inline fn peek(self: *Self, distance: i32) Value {
    const stack = self.stack.items;
    return stack[stack.len - 1 - distance];
}

inline fn push(self: *Self, value: Value) void {
    self.stack.append(value) catch {};
}

inline fn pop(self: *Self) Value {
    return self.stack.pop();
}

fn isFalsey(value: Value) bool {
    return value.isNil() or (value.isBool() and !value.asBool());
}

fn run(self: *Self) InterpretResult {
    while (true) {
        const instruction = self.readByte();
        switch (@intToEnum(OpCode, instruction)) {
            .Return => {
                var c = self.pop();
                c.print();
                print("\n", .{});
                return InterpretResult.ok;
            },
            .Constant => self.push(self.readConstant()),
            .True => self.push(Value.fromBool(true)),
            .False => self.push(Value.fromBool(false)),
            .Equal => {
                const b = self.pop();
                const a = self.pop();
                self.push(Value.fromBool(a.equals(b)));
            },
            .Nil => self.push(Value.fromNil()),

            .Add => self.binaryOp('+'),
            .Subtract => self.binaryOp('-'),
            .Multiply => self.binaryOp('*'),
            .Divide => self.binaryOp('/'),

            .Greater => self.binaryOp('>'),
            .Less => self.binaryOp('<'),

            .Not => self.push(Value.fromBool(isFalsey(self.pop()))),
            .Negate => {
                if (!self.peek(0).isNumber()) {
                    self.runtimeError("Operand must be a number.");
                    return .runtimeError;
                }
                var v = self.pop();
                switch (v) {
                    .number => |n| self.push(Value.fromF32(-n)),
                    else => unreachable,
                }
            },

            else => unreachable,
        }
    }
    return InterpretResult.ok;
}

// Helper functions
fn binaryOp(self: *Self, comptime op: u8) void {
    var rhs = self.pop();
    var lhs = self.pop();

    switch (op) {
        '+' => self.push(Value.fromF32(lhs.number + rhs.number)),
        '-' => self.push(Value.fromF32(lhs.number - rhs.number)),
        '*' => self.push(Value.fromF32(lhs.number * rhs.number)),
        '/' => self.push(Value.fromF32(lhs.number / rhs.number)),

        '>' => self.push(Value.fromBool(lhs.number > rhs.number)),
        '<' => self.push(Value.fromBool(lhs.number < rhs.number)),
        else => unreachable,
    }
}

inline fn readByte(self: *Self) u8 {
    defer self.ip += 1;
    return self.chunk.?.code.items[self.ip];
}

inline fn readConstant(self: *Self) Value {
    return self.chunk.?.constant_pool.values.items[self.readByte()];
}

test "Simple expression" {
    if (debug.PRINT_CODE or debug.TRACE_EXECUTION) {
        return error.SkipZigTest;
    }

    const source = "-100 + 200 * 2";
    var vm = try Self.init(std.heap.page_allocator);
    defer vm.deinit();
    std.debug.assert(vm.setup_and_go(source) == InterpretResult.ok);
}
