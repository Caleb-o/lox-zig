const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Chunk = @import("Chunk.zig").Chunk;
const OpCode = @import("Chunk.zig").OpCode;
const Value = @import("value.zig").Value;
const Object = @import("Object.zig");
const Token = @import("scanner.zig").Token;
const TokenKind = @import("scanner.zig").TokenKind;
const Scanner = @import("scanner.zig").Scanner;
const debug = @import("debug.zig");
const VM = @import("VM.zig");

pub const CompilerError = enum {
    IncorrectToken,
};

const Parser = struct {
    current: ?Token,
    previous: ?Token,
    hadError: bool,
    panicMode: bool,

    pub fn create() Parser {
        return .{
            .current = null,
            .previous = null,
            .hadError = false,
            .panicMode = false,
        };
    }
};

const Precedence = enum {
    None,
    Assignment, // =
    Or, // or
    And, // and
    Equality, // == !=
    Comparison, // < > <= >=
    Term, // + -
    Factor, // * /
    Unary, // ! -
    Call, // . ()
    Primary,

    pub fn next(self: Precedence) Precedence {
        return @intToEnum(Precedence, @enumToInt(self) + 1);
    }
};

pub const Self = @This();

// Fields
compilingChunk: ?*Chunk,
scanner: ?Scanner,
parser: Parser,
vm: *VM,

pub fn create(vm: *VM) Self {
    return .{
        .compilingChunk = null,
        .scanner = null,
        .parser = Parser.create(),
        .vm = vm,
    };
}

pub fn compile(self: *Self, chunk: *Chunk, source: []const u8) bool {
    self.parser.hadError = false;
    self.parser.panicMode = false;

    self.compilingChunk = chunk;
    self.scanner = Scanner.init(source);

    self.advance();
    self.expression();
    self.consume(.Eof, "Expect end of expression.");
    self.end();

    return !self.parser.hadError;
}

fn getPrecedence(tokenType: TokenKind) Precedence {
    return switch (tokenType) {
        // Single-character tokens.
        .LeftParen => .Call,
        .RightParen, .LeftBrace, .RightBrace, .Comma => .None,
        .Dot => .Call,
        .Minus, .Plus => .Term,
        .Semicolon => .None,
        .Slash, .Star => .Factor,

        // One or two character tokens.
        .BangEqual, .EqualEqual => .Equality,
        .Greater, .GreaterEqual, .Less, .LessEqual => .Comparison,
        .Bang, .Equal => .None,

        // Literals.
        .Identifier, .String, .Number => .None,

        // Keywords.
        .And => .And,
        .Or => .Or,
        .Class, .Else, .False, .For, .Fun, .If, .Nil => .None,
        .Print, .Return, .Super, .This, .True, .Var, .While, .Error => .None,
        .Eof => .None,
    };
}

inline fn currentChunk(self: *Self) *Chunk {
    return self.compilingChunk;
}

fn consume(self: *Self, kind: TokenKind, msg: []const u8) void {
    if (self.parser.current.?.kind == kind) {
        self.advance();
        return;
    }

    self.errorAtCurrent(msg);
}

// FIXME: Handle errors
inline fn emitByte(self: *Self, op: OpCode) void {
    self.compilingChunk.?.write(op, self.parser.previous.?.line) catch {};
}

// FIXME: Handle errors
fn emitBytes(self: *Self, op1: OpCode, op2: OpCode) void {
    self.compilingChunk.?.write(op1, self.parser.previous.?.line) catch {};
    self.compilingChunk.?.write(op2, self.parser.previous.?.line) catch {};
}

inline fn emitReturn(self: *Self) void {
    self.emitByte(.Return);
}

inline fn end(self: *Self) void {
    self.emitReturn();

    if (debug.PRINT_CODE) {
        if (!self.parser.hadError) {
            debug.disassembleChunk(self.compilingChunk.?, "Code");
        }
    }
}

fn binary(self: *Self) void {
    const operatorKind = self.parser.previous.?.kind;
    self.parsePrecendence(getPrecedence(operatorKind).next());

    switch (operatorKind) {
        .Plus => self.emitByte(.Add),
        .Minus => self.emitByte(.Subtract),
        .Star => self.emitByte(.Multiply),
        .Slash => self.emitByte(.Divide),

        .BangEqual => self.emitByte(.Not),
        .EqualEqual => self.emitByte(.Equal),
        .Greater => self.emitByte(.Greater),
        .GreaterEqual => self.emitBytes(.Greater, .Not),
        .Less => self.emitByte(.Less),
        .LessEqual => self.emitBytes(.Less, .Not),
        else => unreachable,
    }
}

fn literal(self: *Self) void {
    switch (self.parser.previous.?.kind) {
        .False => self.emitByte(.False),
        .True => self.emitByte(.True),
        .Nil => self.emitByte(.Nil),
        else => unreachable,
    }
}

fn groupedExpression(self: *Self) void {
    self.expression();
    self.consume(.RightParen, "Expect ')' after expression.");
}

fn stringValue(self: *Self) Value {
    const source = self.parser.previous.?.lexeme[1 .. self.parser.previous.?.lexeme.len - 1];
    return Value.fromObject(
        &Object.ObjectString.copy(self.vm, source).object,
    );
}

inline fn string(self: *Self) void {
    self.emitConstant(self.stringValue());
}

fn number(self: *Self) void {
    // TODO: Handle error properly
    const float = (std.fmt.parseFloat(f32, self.parser.previous.?.lexeme) catch {
        std.debug.panic("Could not parse as f32 '{s}'", .{self.parser.previous.?.lexeme});
    });
    self.emitConstant(Value.fromF32(float));
}

fn unary(self: *Self) void {
    const operatorKind = self.parser.previous.?.kind;
    // Compile operand
    self.parsePrecendence(.Unary);

    switch (operatorKind) {
        .Bang => self.emitByte(.Not),
        .Minus => self.emitByte(.Negate),
        else => unreachable,
    }
}

fn parsePrecendence(self: *Self, precedence: Precedence) void {
    self.advance();
    self.prefix(self.parser.previous.?.kind);

    while (@enumToInt(precedence) <= @enumToInt(getPrecedence(self.parser.current.?.kind))) {
        self.advance();
        self.infix(self.parser.previous.?.kind);
    }
}

fn prefix(self: *Self, kind: TokenKind) void {
    switch (kind) {
        .LeftParen => self.groupedExpression(),
        .Bang, .Minus => self.unary(),

        .Nil, .True, .False => self.literal(),

        .String => self.string(),
        .Number => self.number(),

        // Should error instead
        else => unreachable,
    }
}

fn infix(self: *Self, kind: TokenKind) void {
    switch (kind) {
        .Plus, .Minus, .Star, .Slash => self.binary(),

        .BangEqual, .EqualEqual => self.binary(),
        .Greater, .GreaterEqual => self.binary(),
        .Less, .LessEqual => self.binary(),
        // Should error instead
        else => unreachable,
    }
}

inline fn emitConstant(self: *Self, value: Value) void {
    self.emitBytes(.Constant, @intToEnum(OpCode, self.makeConstant(value)));
}

fn makeConstant(self: *Self, value: Value) u8 {
    // TODO: Handle error
    var constant = self.compilingChunk.?.addConstant(value) catch {
        self.@"error"("Too many constants in one chunk.");
        return 0;
    };

    return constant;
}

fn errorAtCurrent(self: *Self, msg: []const u8) void {
    self.errorAt(&self.parser.current.?, msg);
}

fn @"error"(self: *Self, msg: []const u8) void {
    self.errorAt(&self.parser.previous.?, msg);
}

fn errorAt(self: *Self, token: *Token, msg: []const u8) void {
    if (self.parser.panicMode) return;
    self.parser.panicMode = true;

    std.debug.print("[line {d}] Error", .{token.line});

    if (token.kind == .Eof) {
        std.debug.print(" at end", .{});
    } else if (token.kind == .Error) {
        // Nothing
    } else {
        std.debug.print(" at '{s}'", .{token.lexeme});
    }

    std.debug.print(": {s}\n", .{msg});
    self.parser.hadError = true;
}

fn advance(self: *Self) void {
    self.parser.previous = self.parser.current;

    while (true) {
        self.parser.current = self.scanner.?.getToken();
        if (self.parser.current.?.kind != .Error) break;
        self.errorAtCurrent(self.parser.current.?.lexeme);
    }
}

fn expression(self: *Self) void {
    self.parsePrecendence(.Assignment);
}
