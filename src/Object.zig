const std = @import("std");
const Allocator = std.mem.Allocator;
const Value = @import("value.zig").Value;
const Chunk = @import("Chunk.zig").Chunk;
const VM = @import("VM.zig");

const Self = @This();

pub const ObjectKind = enum {
    string,
    function,
    nativeFunction,
    closure,
};

// Fields
kind: ObjectKind,
next: ?*Self,

pub fn allocate(vm: *VM, comptime T: type, kind: ObjectKind) *Self {
    // TODO: Handle errors
    const ptr = vm.allocator.create(T) catch unreachable;

    ptr.object = Self{
        .kind = kind,
        .next = vm.objects,
    };

    vm.objects = &ptr.object;

    return &ptr.object;
}

pub fn destroy(self: *Self, vm: *VM) void {
    switch (self.kind) {
        .string => self.asString().destroy(vm),
        .function => self.asFunction().destroy(vm),
        .nativeFunction => self.asNativeFunction().destroy(vm),
        .closure => self.asClosure().destroy(vm),
    }
}

pub const ObjectString = struct {
    object: Self,
    hash: u32,
    chars: []const u8,

    const String = @This();

    pub fn create(vm: *VM, buffer: []const u8) *String {
        const hash = getHash(buffer);

        // Find an interned string
        if (vm.strings.findString(buffer, hash)) |str| {
            vm.allocator.free(buffer);
            return str;
        }

        const object = Self.allocate(vm, String, .string);
        const str = object.asString();
        str.object = object.*;
        str.chars = buffer;
        str.hash = hash;

        _ = vm.strings.set(str, Value.fromBool(true));

        return str;
    }

    pub fn copy(vm: *VM, source: []const u8) *String {
        const buffer = vm.allocator.alloc(u8, source.len) catch unreachable;
        std.mem.copy(u8, buffer, source);
        return String.create(vm, buffer);
    }

    pub fn destroy(self: *String, vm: *VM) void {
        vm.allocator.free(self.chars);
        vm.allocator.destroy(self);
    }

    fn getHash(buffer: []const u8) u32 {
        var hash: u32 = 2166136261;
        for (buffer) |byte| {
            hash ^= @as(u32, byte);
            hash *%= 16777619;
        }
        return hash;
    }
};

pub const ObjectFunction = struct {
    object: Self,
    arity: u8,
    upvalueCount: u32,
    chunk: Chunk,
    identifier: ?*ObjectString,

    const Function = @This();

    pub fn create(vm: *VM) *Function {
        const object = Self.allocate(vm, Function, .function);
        const func = object.asFunction();
        func.arity = 0;
        func.upvalueCount = 0;
        func.identifier = null;
        func.chunk = Chunk.init(vm.allocator);

        return func;
    }

    pub fn destroy(self: *Function, vm: *VM) void {
        // NOTE: The name will be handled by the GC
        self.chunk.deinit();
        vm.allocator.destroy(self);
    }
};

pub const NativeFn = *const fn (args: []Value) Value;

pub const ObjectNativeFn = struct {
    object: Self,
    function: NativeFn,

    const Native = @This();

    pub fn create(vm: *VM, function: NativeFn) *Native {
        const object = Self.allocate(vm, Native, .nativeFunction);
        const func = object.asNativeFunction();
        func.function = function;

        return func;
    }

    pub fn destroy(self: *Native, vm: *VM) void {
        vm.allocator.destroy(self);
    }
};

pub const ObjectClosure = struct {
    object: Self,
    function: *ObjectFunction,

    const Closure = @This();

    pub fn create(vm: *VM, function: *ObjectFunction) *Closure {
        const object = Self.allocate(vm, Closure, .closure);
        const func = object.asClosure();
        func.function = function;

        return func;
    }

    pub fn destroy(self: *Closure, vm: *VM) void {
        vm.allocator.destroy(self);
    }
};

// Check
pub inline fn isString(self: *Self) bool {
    return self.kind == .string;
}

pub inline fn isFunction(self: *Self) bool {
    return self.kind == .function;
}

pub inline fn isNativeFunction(self: *Self) bool {
    return self.kind == .nativeFunction;
}

pub inline fn isClosure(self: *Self) bool {
    return self.kind == .closure;
}

// "Cast"
pub fn asString(self: *Self) *ObjectString {
    std.debug.assert(self.isString());
    return @fieldParentPtr(ObjectString, "object", self);
}

pub fn asFunction(self: *Self) *ObjectFunction {
    std.debug.assert(self.isFunction());
    return @fieldParentPtr(ObjectFunction, "object", self);
}

pub fn asNativeFunction(self: *Self) *ObjectNativeFn {
    std.debug.assert(self.isNativeFunction());
    return @fieldParentPtr(ObjectNativeFn, "object", self);
}

pub fn asClosure(self: *Self) *ObjectClosure {
    std.debug.assert(self.isClosure());
    return @fieldParentPtr(ObjectClosure, "object", self);
}
