const std = @import("std");
const EnvMap = std.process.EnvMap;
const page_allocator = std.heap.page_allocator;
const Allocator = std.mem.Allocator;
const Chunk = @import("Chunk.zig").Chunk;
const OpCode = @import("Chunk.zig").OpCode;
const Value = @import("value.zig").Value;
const VM = @import("VM.zig");

pub fn main(init: std.process.Init) !void {
    var gpa = std.heap.DebugAllocator(.{}){};
    const allocator = gpa.allocator();
    const args = try init.minimal.args.toSlice(init.arena.allocator());

    defer {
        if (gpa.deinit() == .leak) {
            std.debug.panic("Internal Error: Memory leaked\n", .{});
        }
    }

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    const errAlloc = arena.allocator();

    if (args.len == 1) {
        try repl(init.io, allocator, errAlloc);
    } else if (args.len == 2) {
        try runFile(init.io, allocator, errAlloc, args[1]);
    } else {
        std.debug.print("Usage: clox [path]\n", .{});
    }
}

fn nextLine(reader: *std.Io.Reader) !?[]const u8 {
    const line = (try reader.takeDelimiter(
        '\n',
    )) orelse return null;
    // trim annoying windows-only carriage return character
    if (@import("builtin").os.tag == .windows) {
        return std.mem.trimEnd(u8, line, "\r");
    } else {
        return line;
    }
}

fn repl(io: std.Io, allocator: Allocator, errAlloc: Allocator) !void {
    var vm = try VM.init(io, allocator, errAlloc);
    defer vm.deinit();

    while (true) {
        std.debug.print(">>", .{});

        var buffer: [1024]u8 = undefined;
        var stdin = std.Io.File.Reader.init(.stdin(), io, &buffer);

        const input = (try nextLine(&stdin.interface)).?;
        if (input.len == 0) {
            return;
        }
        _ = vm.setup_and_go(input);
    }
}

fn readFile(io: std.Io, allocator: Allocator, path: [:0]const u8) ![]u8 {
    return try std.Io.Dir.cwd().readFileAlloc(
        io,
        path,
        allocator,
        .unlimited,
    );
}

fn runFile(io: std.Io, allocator: Allocator, errAlloc: Allocator, path: [:0]const u8) !void {
    const source = try readFile(io, allocator, path);
    var vm = try VM.init(io, allocator, errAlloc);

    defer {
        allocator.free(source);
        vm.deinit();
    }

    _ = vm.setup_and_go(source);
}
