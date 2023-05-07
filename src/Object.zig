const std = @import("std");
const Allocator = std.mem.Allocator;
const Value = @import("value.zig").Value;
const VM = @import("VM.zig");

const Self = @This();

pub const ObjectKind = enum {
    string,
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

// Check
pub inline fn isString(self: *Self) bool {
    return self.kind == .string;
}

// "Cast"
pub fn asString(self: *Self) *ObjectString {
    std.debug.assert(self.isString());
    return @fieldParentPtr(ObjectString, "object", self);
}
