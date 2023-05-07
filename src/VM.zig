const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Chunk = @import("Chunk.zig").Chunk;
const OpCode = @import("Chunk.zig").OpCode;
const Value = @import("value.zig").Value;
const Object = @import("Object.zig");
const Compiler = @import("Compiler.zig");
const print = std.debug.print;
const debug = @import("debug.zig");
const Table = @import("Table.zig");

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
strings: Table,
objects: ?*Object,

// Methods

pub fn init(allocator: Allocator) !Self {
    return .{
        .allocator = allocator,
        .chunk = null,
        .ip = 0,
        .stack = try ArrayList(Value).initCapacity(allocator, 256),
        .strings = Table.init(allocator),
        .objects = null,
    };
}

pub fn deinit(self: *Self) void {
    self.stack.deinit();
    self.strings.deinit();
    self.freeObjects();
    self.objects = null;
}

pub fn setup_and_go(self: *Self, source: []const u8) InterpretResult {
    var chunk = Chunk.init(self.allocator);
    defer chunk.deinit();

    var compiler = Compiler.create(self);
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

fn concatenate(self: *Self, lhs: *Object, rhs: *Object) void {
    switch (lhs.kind) {
        .string => switch (rhs.kind) {
            .string => {
                // Make visible to GC
                self.push(Value.fromObject(lhs));
                self.push(Value.fromObject(rhs));

                const lstr = lhs.asString();
                const rstr = rhs.asString();
                // TODO: Handle errors
                const buffer = self.allocator.alloc(u8, lstr.chars.len + rstr.chars.len) catch unreachable;

                std.mem.copy(u8, buffer[0..lstr.chars.len], lstr.chars);
                std.mem.copy(u8, buffer[lstr.chars.len..], rstr.chars);

                _ = self.pop();
                _ = self.pop();

                self.push(Value.fromObject(&Object.ObjectString.create(self, buffer).object));
            },
        },
    }
}

fn freeObjects(self: *Self) void {
    var object = self.objects;
    while (object) |o| {
        const next = o.next;
        o.destroy(self);
        object = next;
    }
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

            .Add => {
                const rhs = self.pop();
                const lhs = self.pop();

                if (lhs.isObject() and rhs.isObject()) {
                    self.concatenate(lhs.asObject(), rhs.asObject());
                } else if (lhs.isNumber() and rhs.isNumber()) {} else {
                    self.runtimeError("Operands must be two numbers or two strings.");
                    return .runtimeError;
                }
            },
            .Subtract => self.binaryOp('-'),
            .Multiply => self.binaryOp('*'),
            .Divide => self.binaryOp('/'),

            .Greater => self.binaryOp('>'),
            .Less => self.binaryOp('<'),

            .Not => self.push(Value.fromBool(self.pop().isFalsey())),
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

test "Simple Expression" {
    if (debug.PRINT_CODE or debug.TRACE_EXECUTION) {
        return error.SkipZigTest;
    }

    const source = "-100 + 200 * 2";
    var vm = try Self.init(std.testing.allocator);
    defer vm.deinit();
    std.debug.assert(vm.setup_and_go(source) == InterpretResult.ok);
}

test "String Equality" {
    if (debug.PRINT_CODE or debug.TRACE_EXECUTION) {
        return error.SkipZigTest;
    }

    const source =
        \\"Hello" == "Hello"
    ;
    var vm = try Self.init(std.testing.allocator);
    defer vm.deinit();
    std.debug.assert(vm.setup_and_go(source) == InterpretResult.ok);
}

test "String Concatenation" {
    if (debug.PRINT_CODE or debug.TRACE_EXECUTION) {
        return error.SkipZigTest;
    }

    const source =
        \\"Hello, " + "World!"
    ;
    var vm = try Self.init(std.testing.allocator);
    defer vm.deinit();
    std.debug.assert(vm.setup_and_go(source) == InterpretResult.ok);
}
