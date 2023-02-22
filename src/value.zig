const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

pub const Value = union(enum) {
    double: f32,

    const Self = @This();

    pub fn print(self: *Self) void {
        switch (self.*) {
            .double => |v| std.debug.print("{d:.6}", .{v}),
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
