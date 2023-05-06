const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Chunk = @import("Chunk.zig").Chunk;
const OpCode = @import("Chunk.zig").OpCode;
const Value = @import("value.zig").Value;
const Scanner = @import("scanner.zig").Scanner;
const print = std.debug.print;

pub const Self = @This();

// Fields

pub fn compile(self: *Self, source: []const u8) void {
    _ = self;
    var scanner = Scanner.init(source);

    while (true) {
        var token = scanner.getToken();
        std.debug.print("'{s}' {}\n", .{ token.lexeme, token.line });
        if (token.kind == .Error or token.kind == .Eof) break;
    }
}
