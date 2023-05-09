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

const expect = std.testing.expect;

pub const Self = @This();

pub const InterpretResult = enum(u8) {
    ok,
    compilerError,
    runtimeError,
};

const CallFrame = struct {
    function: *Object.ObjectFunction,
    ip: usize,
    slotStart: usize,

    pub fn create(function: *Object.ObjectFunction, slotStart: usize) CallFrame {
        return .{
            .function = function,
            .ip = 0,
            .slotStart = slotStart,
        };
    }
};

// Fields
allocator: Allocator,
errAlloc: Allocator,
stack: ArrayList(Value),
frames: ArrayList(CallFrame),
globals: Table,
strings: Table,
objects: ?*Object,

// Methods

pub fn init(allocator: Allocator, errAlloc: Allocator) !Self {
    return .{
        .allocator = allocator,
        .errAlloc = errAlloc,
        .stack = try ArrayList(Value).initCapacity(allocator, 256),
        .frames = try ArrayList(CallFrame).initCapacity(allocator, 8),
        .globals = Table.init(allocator),
        .strings = Table.init(allocator),
        .objects = null,
    };
}

pub fn deinit(self: *Self) void {
    self.stack.deinit();
    self.globals.deinit();
    self.strings.deinit();
    self.frames.deinit();
    self.freeObjects();
    self.objects = null;
}

pub fn setup_and_go(self: *Self, source: []const u8) InterpretResult {
    var compiler = Compiler.create(self);

    self.defineNative("clock", nativeClock);

    if (compiler.compile(source)) |func| {
        self.push(Value.fromObject(&func.object));
        _ = self.call(func, 0);
        const result = self.run();
        _ = self.pop();
        return result;
    }

    return .compilerError;
}

// Native
fn nativeClock(args: []Value) Value {
    _ = args;
    return Value.fromF32(@intToFloat(f32, std.time.milliTimestamp()));
}

inline fn pushFrame(self: *Self, frame: CallFrame) void {
    // TODO: Handle Errors
    self.frames.append(frame) catch unreachable;
}

inline fn resetStack(self: *Self) void {
    self.stack.clearAndFree();
}

fn defineNative(self: *Self, name: []const u8, function: Object.NativeFn) void {
    self.push(Value.fromObject(&Object.ObjectString.copy(self, name).object));
    self.push(Value.fromObject(&Object.ObjectNativeFn.create(self, function).object));

    _ = self.globals.set(self.stack.items[0].asObject().asString(), self.stack.items[1]);
    _ = self.pop();
    _ = self.pop();
}

fn runtimeError(self: *Self, msg: []const u8) void {
    const frame = self.currentFrame();
    const line = frame.function.chunk.findOpcodeLine(frame.ip);
    std.debug.print("Error: {s} [line {d}]\n", .{ msg, line });

    var idx: isize = @intCast(isize, self.frames.items.len) - 1;
    while (idx >= 0) : (idx -= 1) {
        const stackFrame = &self.frames.items[@intCast(usize, idx)];
        const function = stackFrame.function;
        const funcLine = function.chunk.findOpcodeLine(stackFrame.ip - 1);

        std.debug.print("[line {d}] in ", .{funcLine});
        if (function.identifier) |identifier| {
            std.debug.print("{s}()\n", .{identifier.chars});
        } else {
            std.debug.print("script\n", .{});
        }
    }

    self.resetStack();
}

fn runtimeErrorAlloc(self: *Self, comptime fmt: []const u8, args: anytype) void {
    // TODO: Handle Errors
    const msg = std.fmt.allocPrint(self.errAlloc, fmt, args) catch unreachable;
    self.runtimeError(msg);
}

inline fn peek(self: *Self, distance: i32) Value {
    const stack = self.stack.items;
    return stack[stack.len - 1 - @intCast(usize, distance)];
}

inline fn push(self: *Self, value: Value) void {
    self.stack.append(value) catch unreachable;

    // std.debug.print("\nStack: ", .{});
    // for (self.stack.items) |*item| {
    //     item.print();
    //     std.debug.print(" ", .{});
    // }
    // std.debug.print("\n", .{});
}

inline fn pop(self: *Self) Value {
    return self.stack.pop();
}

fn call(self: *Self, func: *Object.ObjectFunction, argCount: usize) bool {
    if (func.arity != argCount) {
        self.runtimeErrorAlloc(
            "Function '{s}' expected {d} arguments, but received {d}.",
            .{ func.identifier.?.chars, func.arity, argCount },
        );
        return false;
    }

    std.debug.assert(self.stack.items.len >= 1);
    self.pushFrame(CallFrame.create(
        func,
        self.stack.items.len - 1 - argCount,
    ));
    return true;
}

fn callValue(self: *Self, callee: Value, argCount: usize) bool {
    if (callee.isObject()) {
        switch (callee.asObject().kind) {
            .function => return self.call(callee.asObject().asFunction(), argCount),
            .nativeFunction => {
                const native = callee.asObject().asNativeFunction();
                const args = self.stack.items[self.stack.items.len - 1 - argCount ..];

                const result = native.function(args);

                for (0..argCount) |_| {
                    _ = self.pop();
                }

                self.push(result);
                return true;
            },
            else => {},
        }
    }

    self.runtimeError("Can only call functions and classes.");
    return false;
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
            else => unreachable,
        },
        else => unreachable,
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

inline fn currentFrame(self: *Self) *CallFrame {
    return &self.frames.items[self.frames.items.len - 1];
}

fn run(self: *Self) InterpretResult {
    var frame = self.currentFrame();

    while (true) {
        const instruction = self.readByte();
        switch (@intToEnum(OpCode, instruction)) {
            .Loop => {
                const offset = self.readShort();
                frame.ip -= @intCast(usize, offset);
            },

            .Jump => {
                const offset = self.readShort();
                frame.ip += @intCast(usize, offset);
            },

            .JumpIfFalse => {
                const offset = self.readShort();
                frame.ip += @intCast(usize, @boolToInt(self.peek(0).isFalsey())) * offset;
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

            .Pop => _ = self.pop(),

            .GetLocal => {
                const slot = self.readByte();
                self.push(self.stack.items[frame.slotStart + slot]);
            },

            .SetLocal => {
                const slot = self.readByte();
                self.stack.items[frame.slotStart + slot] = self.peek(0);
            },

            .GetGlobal => {
                const name = self.readString();
                if (self.globals.get(name)) |value| {
                    self.push(value);
                    continue;
                }

                self.runtimeErrorAlloc("Undefined variable '{s}'.", .{name.chars});
                return .runtimeError;
            },

            .DefineGlobal => {
                const name = self.readString();
                _ = self.globals.set(name, self.peek(0));
                _ = self.pop();
            },

            .SetGlobal => {
                const name = self.readString();
                if (self.globals.set(name, self.peek(0))) {
                    _ = self.globals.delete(name);
                    self.runtimeErrorAlloc("Undefined variable '{s}.", .{name.chars});
                    return .runtimeError;
                }
            },

            .Add => {
                var rhs = self.pop();
                var lhs = self.pop();

                if (lhs.isObject() and rhs.isObject()) {
                    self.concatenate(lhs.asObject(), rhs.asObject());
                } else if (lhs.isNumber() and rhs.isNumber()) {
                    self.push(Value.fromF32(lhs.asNumber() + rhs.asNumber()));
                } else {
                    self.runtimeError("Operands must be two numbers or two strings.");
                    lhs.print();
                    std.debug.print(" ", .{});
                    rhs.print();
                    std.debug.print("\n", .{});
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

            .Call => {
                const argCount = self.readByte();
                if (!self.callValue(self.peek(argCount), argCount)) {
                    return .runtimeError;
                }
                // Update frame pointer
                frame = self.currentFrame();
            },

            .Print => {
                var value = self.pop();
                value.print();
                std.debug.print("\n", .{});
            },

            .Return => {
                const result = self.pop();
                const oldFrame = self.frames.pop();
                if (self.frames.items.len == 0) {
                    _ = self.pop();
                    return .ok;
                }

                const diff = self.stack.items.len - oldFrame.slotStart;
                self.stack.resize(self.stack.items.len - diff) catch unreachable;

                frame = self.currentFrame();
                self.push(result);
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
    const frame = self.currentFrame();
    defer frame.ip += 1;
    return frame.function.chunk.code.items[frame.ip];
}

inline fn readShort(self: *Self) u16 {
    const frame = self.currentFrame();
    const code = frame.function.chunk.code;
    defer frame.ip += 2;

    // NOTE: Bitshifts on unsigned values is UB, so we must cast so a signed type
    const left = @intCast(u8, @intCast(i16, code.items[frame.ip]) << 8);
    return @intCast(u16, (left | code.items[frame.ip + 1]));
}

inline fn readConstant(self: *Self) Value {
    const frame = self.currentFrame();
    return frame.function.chunk.constant_pool.values.items[self.readByte()];
}

inline fn readString(self: *Self) *Object.ObjectString {
    return self.readConstant().asObject().asString();
}

test "Simple Expression" {
    if (debug.PRINT_CODE or debug.TRACE_EXECUTION) {
        return error.SkipZigTest;
    }

    const source = "-100 + 200 * 2;";
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const errAlloc = arena.allocator();
    var vm = try Self.init(std.testing.allocator, errAlloc);
    defer {
        arena.deinit();
        vm.deinit();
    }
    try expect(vm.setup_and_go(source) == InterpretResult.ok);
}

test "String Equality" {
    if (debug.PRINT_CODE or debug.TRACE_EXECUTION) {
        return error.SkipZigTest;
    }

    const source =
        \\"Hello" == "Hello";
    ;
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const errAlloc = arena.allocator();
    var vm = try Self.init(std.testing.allocator, errAlloc);
    defer {
        arena.deinit();
        vm.deinit();
    }
    try expect(vm.setup_and_go(source) == InterpretResult.ok);
}

test "String Concatenation" {
    if (debug.PRINT_CODE or debug.TRACE_EXECUTION) {
        return error.SkipZigTest;
    }

    const source =
        \\"Hello, " + "World!";
    ;
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const errAlloc = arena.allocator();
    var vm = try Self.init(std.testing.allocator, errAlloc);
    defer {
        arena.deinit();
        vm.deinit();
    }
    try expect(vm.setup_and_go(source) == InterpretResult.ok);
}

test "Global variables" {
    if (debug.PRINT_CODE or debug.TRACE_EXECUTION) {
        return error.SkipZigTest;
    }

    const source =
        \\var foo = "Hello!";
        \\print foo;
    ;
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const errAlloc = arena.allocator();
    var vm = try Self.init(std.testing.allocator, errAlloc);
    defer {
        arena.deinit();
        vm.deinit();
    }
    try expect(vm.setup_and_go(source) == InterpretResult.ok);
}

test "Invalid assignment target" {
    if (debug.PRINT_CODE or debug.TRACE_EXECUTION) {
        return error.SkipZigTest;
    }

    const source =
        \\var a = 1; var b = 1; var c = 1; var d = 1;
        \\a * b = c + d;
    ;
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const errAlloc = arena.allocator();
    var vm = try Self.init(std.testing.allocator, errAlloc);
    defer {
        arena.deinit();
        vm.deinit();
    }
    try expect(vm.setup_and_go(source) == InterpretResult.compilerError);
}
