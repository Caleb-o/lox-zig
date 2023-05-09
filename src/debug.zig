const std = @import("std");
const print = std.debug.print;

const Chunk = @import("Chunk.zig").Chunk;
const OpCode = Chunk.OpCode;
const value = @import("value.zig");

pub const PRINT_CODE = true;
pub const TRACE_EXECUTION = false;

pub fn disassembleChunk(chunk: *Chunk, name: []const u8) void {
    print("=== {s} :: {d} ===\n", .{ name, chunk.code.items.len });

    var offset: u32 = 0;
    while (offset < chunk.code.items.len) {
        offset = disassembleInstruction(chunk, offset);
    }
}

fn disassembleInstruction(chunk: *Chunk, offset: u32) u32 {
    print("{d:0>4} ", .{offset});

    const line_here = chunk.findOpcodeLine(offset);

    if (offset > 0 and line_here == chunk.findOpcodeLine(offset - 1)) {
        print("   | ", .{});
    } else {
        print("{d:>4} ", .{line_here});
    }

    const code = @intToEnum(OpCode, chunk.code.items[offset]);
    return switch (code) {
        .Constant => constantInstruction("OP_CONSTANT", chunk, offset),

        .True => simpleInstruction("OP_TRUE", offset),
        .False => simpleInstruction("OP_FALSE", offset),
        .Nil => simpleInstruction("OP_NIL", offset),

        .Pop => simpleInstruction("OP_POP", offset),

        .GetLocal => byteInstruction("OP_GET_LOCAL", chunk, offset),
        .SetLocal => byteInstruction("OP_SET_LOCAL", chunk, offset),

        .GetGlobal => constantInstruction("OP_GET_GLOBAL", chunk, offset),
        .DefineGlobal => constantInstruction("OP_DEFINE_GLOBAL", chunk, offset),
        .SetGlobal => constantInstruction("OP_SET_GLOBAL", chunk, offset),
        .GetUpvalue => byteInstruction("OP_GET_UPVALUE", chunk, offset),
        .SetUpvalue => byteInstruction("OP_SET_UPVALUE", chunk, offset),

        .Add => simpleInstruction("OP_ADD", offset),
        .Subtract => simpleInstruction("OP_SUBTRACT", offset),
        .Multiply => simpleInstruction("OP_MULTIPLY", offset),
        .Divide => simpleInstruction("OP_DIVIDE", offset),

        .Greater => simpleInstruction("OP_GREATER", offset),
        .Less => simpleInstruction("OP_LESS", offset),
        .Equal => simpleInstruction("OP_EQUAL", offset),

        .Not => simpleInstruction("OP_NOT", offset),
        .Negate => simpleInstruction("OP_NEGATE", offset),
        .Print => simpleInstruction("OP_PRINT", offset),

        .Loop => jumpInstruction("OP_LOOP", -1, chunk, offset),
        .Jump => jumpInstruction("OP_JUMP", 1, chunk, offset),
        .JumpIfFalse => jumpInstruction("OP_JUMP_IF_FALSE", 1, chunk, offset),

        .Closure => {
            var newOffset = offset + 1;
            const constant = chunk.code.items[newOffset];
            newOffset += 1;
            std.debug.print("{s:<16} {d:0>4} ", .{ "OP_CLOSURE", constant });

            var val = chunk.constant_pool.values.items[constant];
            val.print();

            std.debug.print("\n", .{});

            const function = val.asObject().asFunction();

            for (0..function.upvalueCount) |_| {
                const isLocal = chunk.code.items[newOffset];
                newOffset += 1;
                const index = chunk.code.items[newOffset];
                newOffset += 1;

                std.debug.print(
                    "{d:0>4}      |                     {s} {d}\n",
                    .{ newOffset - 2, if (isLocal == 1) "local" else "upvalue", index },
                );
            }

            return newOffset;
        },
        .Call => byteInstruction("OP_CALL", chunk, offset),
        .Return => simpleInstruction("OP_RETURN", offset),

        else => unreachable,
    };
}

fn constantInstruction(comptime name: []const u8, chunk: *Chunk, offset: u32) u32 {
    const constant = chunk.code.items[offset + 1];
    print("{s:<16} {d:>4} '", .{ name, constant });
    chunk.constant_pool.values.items[constant].print();
    print("'\n", .{});

    return offset + 2;
}

fn simpleInstruction(comptime name: []const u8, offset: u32) u32 {
    print("{s:<16}\n", .{name});
    return offset + 1;
}

fn byteInstruction(comptime name: []const u8, chunk: *Chunk, offset: u32) u32 {
    const slot = chunk.code.items[offset + 1];
    std.debug.print("{s:<16} {d:4}\n", .{ name, slot });
    return offset + 2;
}

fn jumpInstruction(comptime name: []const u8, sign: i32, chunk: *Chunk, offset: u32) u32 {
    const jump = (@intCast(i16, chunk.code.items[offset + 1]) << 8) | @intCast(i16, chunk.code.items[offset + 2]);
    std.debug.print("{s:<16} {d:4} -> {d}\n", .{
        name,
        offset,
        @intCast(i32, offset + 3) + sign * jump,
    });
    return offset + 3;
}
