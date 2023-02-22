const std = @import("std");
const print = std.debug.print;

const chunkm = @import("Chunk.zig");
const OpCode = chunkm.OpCode;
const valuem = @import("value.zig");

pub fn disassembleChunk(chunk: *chunkm.Chunk, name: []const u8) void {
    print("=== {s} ===\n", .{name});

    var offset: u32 = 0;
    while (offset < chunk.code.items.len) {
        offset = disassembleInstruction(chunk, offset);
    }
}

fn disassembleInstruction(chunk: *chunkm.Chunk, offset: u32) u32 {
    print("{d:0>4} ", .{offset});

    const line_here = chunk.findOpcodeLine(offset);

    if (offset > 0 and line_here == chunk.findOpcodeLine(offset - 1)) {
        print("   | ", .{});
    } else {
        print("{d:>4} ", .{line_here});
    }

    const code = @intToEnum(OpCode, chunk.code.items[offset]);
    switch (code) {
        OpCode.constant => return constantInstruction("OP_CONSTANT", chunk, offset),
        OpCode.@"return" => return simpleInstruction("OP_RETURN", offset),
    }
}

fn constantInstruction(name: []const u8, chunk: *chunkm.Chunk, offset: u32) u32 {
    const constant = chunk.code.items[offset + 1];
    print("{s} {d:>4} '", .{ name, constant });
    chunk.constant_pool.values.items[constant].print();
    print("'\n", .{});

    return offset + 2;
}

fn simpleInstruction(name: []const u8, offset: u32) u32 {
    print("{s}\n", .{name});
    return offset + 1;
}
