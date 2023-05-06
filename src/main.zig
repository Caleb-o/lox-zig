const std = @import("std");
const EnvMap = std.process.EnvMap;
const page_allocator = std.heap.page_allocator;
const Allocator = std.mem.Allocator;
const Chunk = @import("Chunk.zig").Chunk;
const OpCode = @import("Chunk.zig").OpCode;
const Value = @import("value.zig").Value;
const VM = @import("VM.zig").VM;

pub fn main() !void {
    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    const gpa = general_purpose_allocator.allocator();
    const args = try std.process.argsAlloc(gpa);
    defer _ = general_purpose_allocator.deinit();
    defer std.process.argsFree(gpa, args);

    if (args.len == 1) {
        try repl();
    } else if (args.len == 2) {
        try runFile(gpa, args[1]);
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

fn repl() !void {
    const stdout = std.io.getStdOut();
    const stdin = std.io.getStdIn();

    var vm = try VM.init(page_allocator);
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
        _ = vm.setup(input);
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

    _ = vm.setup(source);
}
