const std = @import("std");
const EnvMap = std.process.EnvMap;
const page_allocator = std.heap.page_allocator;
const Allocator = std.mem.Allocator;
const Chunk = @import("Chunk.zig").Chunk;
const OpCode = @import("Chunk.zig").OpCode;
const Value = @import("value.zig").Value;
const VM = @import("VM.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    const args = try std.process.argsAlloc(allocator);
    defer {
        const status = gpa.deinit();
        if (status == .leak) std.debug.panic("Internal Error: Memory leaked\n", .{});
    }
    defer std.process.argsFree(allocator, args);

    if (args.len == 1) {
        try repl(allocator);
    } else if (args.len == 2) {
        try runFile(allocator, args[1]);
    } else {
        std.debug.print("Usage: clox [path]\n", .{});
    }
}

fn nextLine(reader: anytype, buffer: []u8) !?[]const u8 {
    var line = (try reader.readUntilDelimiterOrEof(
        buffer,
        '\n',
    )) orelse return null;
    // trim annoying windows-only carriage return character
    if (@import("builtin").os.tag == .windows) {
        return std.mem.trimRight(u8, line, "\r");
    } else {
        return line;
    }
}

fn repl(allocator: Allocator) !void {
    const stdout = std.io.getStdOut();
    const stdin = std.io.getStdIn();

    var vm = try VM.init(allocator);
    defer vm.deinit();

    while (true) {
        try stdout.writeAll(
            \\>
        );

        var buffer: [1024]u8 = undefined;
        const input = (try nextLine(stdin.reader(), &buffer)).?;
        if (input.len == 0) {
            return;
        }
        _ = vm.setup_and_go(input);
    }
}

fn readFile(allocator: Allocator, path: [:0]u8) ![]u8 {
    const file = try std.fs.cwd().openFile(
        path,
        .{},
    );
    defer file.close();

    const size = (try file.stat()).size;
    const contents = try file.reader().readAllAlloc(allocator, size);
    return contents;
}

fn runFile(allocator: Allocator, path: [:0]u8) !void {
    var source = try readFile(allocator, path);

    var vm = try VM.init(page_allocator);
    defer vm.deinit();
    defer allocator.destroy(source);

    _ = vm.setup_and_go(source);
}
