const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

pub const Value = union(enum) {
    nil: void,
    number: f32,
    boolean: bool,

    const Self = @This();

    pub inline fn fromNil() Self {
        return .{
            .nil = void{},
        };
    }

    pub inline fn fromF32(value: f32) Self {
        return .{
            .number = value,
        };
    }

    pub inline fn fromBool(value: bool) Self {
        return .{
            .boolean = value,
        };
    }

    // Check
    pub fn isNil(self: Self) bool {
        return self == .nil;
    }

    pub fn isNumber(self: Self) bool {
        return self == .number;
    }

    pub fn isBool(self: Self) bool {
        return self == .boolean;
    }

    // "Cast"
    pub fn asNil(self: Self) void {
        std.debug.assert(self.isNil());
        return self.nil;
    }

    pub fn asNumber(self: Self) f32 {
        std.debug.assert(self.isNumber()());
        return self.number;
    }

    pub fn asBool(self: Self) bool {
        std.debug.assert(self.isBool());
        return self.boolean;
    }

    // Utility
    pub fn equals(self: Self, other: Self) bool {
        return switch (self) {
            .nil => switch (other) {
                .nil => true,
                else => false,
            },
            .number => |v| switch (other) {
                .number => |o| v == o,
                else => false,
            },
            .boolean => |v| switch (other) {
                .boolean => |o| v == o,
                else => false,
            },
        };
    }

    pub fn print(self: *Self) void {
        switch (self.*) {
            .nil => std.debug.print("nil", .{}),
            .number => |v| std.debug.print("{d:.6}", .{v}),
            .boolean => |v| std.debug.print("{any}", .{v}),
        }
    }
};

pub const ValueArray = struct {
    values: ArrayList(Value),

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return Self{
            .values = ArrayList(Value).init(allocator),
        };
    }

    pub inline fn deinit(self: *Self) void {
        self.values.deinit();
    }

    pub inline fn write(self: *Self, value: Value) !void {
        try self.values.append(value);
    }
};
