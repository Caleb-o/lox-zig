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
    current: Token,
    previous: Token,
    hadError: bool,
    panicMode: bool,

    pub fn create() Parser {
        return .{
            .current = undefined,
            .previous = undefined,
            .hadError = false,
            .panicMode = false,
        };
    }
};

const Local = struct {
    name: Token,
    depth: i32,

    pub fn create() Local {
        return .{
            .name = undefined,
            .depth = 0,
        };
    }

    pub fn artificial(identifier: []const u8) Local {
        return .{
            .name = Token{
                .kind = .String,
                .lexeme = identifier,
                .line = 0,
            },
            .depth = 0,
        };
    }
};

const UpValue = struct {
    index: u8,
    isLocal: bool,

    pub fn create() UpValue {
        return .{
            .index = 0,
            .isLocal = false,
        };
    }
};

const FunctionKind = enum {
    script,
    function,
};

pub const Compiler = struct {
    enclosing: ?*Compiler,
    function: *Object.ObjectFunction,
    functionKind: FunctionKind,
    locals: [std.math.maxInt(u8) + 1]Local,
    upvalues: [std.math.maxInt(u8) + 1]UpValue,
    count: usize,
    scopeDepth: i32,

    pub fn create(vm: *VM, functionKind: FunctionKind, enclosing: ?*Compiler) Compiler {
        var compiler = .{
            .enclosing = enclosing,
            .function = Object.ObjectFunction.create(vm),
            .functionKind = functionKind,
            .locals = [_]Local{Local.create()} ** (std.math.maxInt(u8) + 1),
            .upvalues = [_]UpValue{UpValue.create()} ** (std.math.maxInt(u8) + 1),
            .count = 1,
            .scopeDepth = 0,
        };

        const local = &compiler.locals[0];
        local.* = Local.artificial("");

        return compiler;
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
current: *Compiler,
scanner: Scanner,
parser: Parser,
vm: *VM,

pub fn create(vm: *VM) Self {
    return .{
        .scanner = undefined,
        .current = undefined,
        .parser = Parser.create(),
        .vm = vm,
    };
}

pub fn compile(self: *Self, source: []const u8) ?*Object.ObjectFunction {
    self.parser.hadError = false;
    self.parser.panicMode = false;

    self.scanner = Scanner.init(source);

    var compiler = Compiler.create(self.vm, .script, null);
    self.setCompiler(&compiler);

    self.advance();

    while (!self.match(.Eof)) {
        self.declaration();
    }

    if (!self.parser.hadError) {
        return self.end();
    }

    return null;
}

inline fn currentChunk(self: *Self) *Chunk {
    return &self.current.function.chunk;
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

fn consume(self: *Self, kind: TokenKind, msg: []const u8) void {
    if (self.parser.current.kind == kind) {
        self.advance();
        return;
    }

    self.errorAtCurrent(msg);
}

inline fn setCompiler(self: *Self, compiler: *Compiler) void {
    self.current = compiler;

    if (self.current.functionKind != .script) {
        self.current.function.identifier =
            Object.ObjectString.copy(self.vm, self.parser.previous.lexeme);
    }
}

// FIXME: Handle errors
inline fn emitByte(self: *Self, op: OpCode) void {
    self.currentChunk().write(op, self.parser.previous.line) catch unreachable;
}

inline fn emitByteU8(self: *Self, op: u8) void {
    self.currentChunk().writeByte(op, self.parser.previous.line) catch unreachable;
}

// FIXME: Handle errors
fn emitBytes(self: *Self, op1: OpCode, op2: OpCode) void {
    self.currentChunk().write(op1, self.parser.previous.line) catch unreachable;
    self.currentChunk().write(op2, self.parser.previous.line) catch unreachable;
}

fn emitBytesU8(self: *Self, op1: OpCode, op2: u8) void {
    self.currentChunk().write(op1, self.parser.previous.line) catch unreachable;
    self.currentChunk().writeByte(op2, self.parser.previous.line) catch unreachable;
}

fn emitJump(self: *Self, op: OpCode) i32 {
    self.emitByte(op);
    self.emitByteU8(0xff);
    self.emitByteU8(0xff);
    return @intCast(i32, self.currentChunk().code.items.len - 2);
}

fn patchJump(self: *Self, offset: i32) void {
    const jump = @intCast(usize, @intCast(i32, self.currentChunk().code.items.len) - offset - 2);

    if (jump > std.math.maxInt(u16)) {
        self.@"error"("Too much code to jump over.");
    }

    self.currentChunk().code.items[@intCast(usize, offset)] = @intCast(u8, (jump >> 8) & 0xff);
    self.currentChunk().code.items[@intCast(usize, offset) + 1] = @intCast(u8, jump & 0xff);
}

inline fn emitReturn(self: *Self) void {
    self.emitBytes(.Nil, .Return);
}

inline fn end(self: *Self) *Object.ObjectFunction {
    self.emitReturn();
    const func = self.current.function;

    if (debug.PRINT_CODE) {
        if (!self.parser.hadError) {
            debug.disassembleChunk(self.currentChunk(), if (func.identifier != null)
                std.fmt.allocPrint(self.vm.errAlloc, "{s}", .{func.identifier.?.chars}) catch unreachable
            else
                "<script>");
        }
    }

    if (self.current.enclosing) |enclosing| {
        self.current = enclosing;
    }
    return func;
}

inline fn beginScope(self: *Self) void {
    self.current.scopeDepth += 1;
}

fn endScope(self: *Self) void {
    self.current.scopeDepth -= 1;

    const compiler = self.current;
    while (compiler.count > 0 and
        compiler.locals[@intCast(usize, compiler.count) - 1].depth > compiler.scopeDepth)
    {
        self.emitByte(.Pop);
        compiler.count -= 1;
    }
}

fn binary(self: *Self) void {
    const operatorKind = self.parser.previous.kind;
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

fn argumentList(self: *Self) u8 {
    var argCount: u8 = 0;

    if (!self.check(.RightParen)) {
        while (true) {
            if (argCount == 255) {
                self.@"error"("Can't have more than 255 arguments in function call.");
            }

            self.expression();
            argCount += 1;

            if (!self.match(.Comma)) break;
        }
    }
    self.consume(.RightParen, "Expect ')' after arguments.");
    return argCount;
}

inline fn call(self: *Self) void {
    const argCount = self.argumentList();
    self.emitBytesU8(.Call, argCount);
}

fn literal(self: *Self) void {
    switch (self.parser.previous.kind) {
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
    const source = self.parser.previous.lexeme[1 .. self.parser.previous.lexeme.len - 1];
    return Value.fromObject(
        &Object.ObjectString.copy(self.vm, source).object,
    );
}

inline fn string(self: *Self) void {
    self.emitConstant(self.stringValue());
}

fn namedVariable(self: *Self, name: *Token, canAssign: bool) void {
    var setop: OpCode = undefined;
    var getop: OpCode = undefined;

    var arg = self.resolveLocal(self.current, name);

    if (arg != -1) {
        getop = .GetLocal;
        setop = .SetLocal;
    } else {
        arg = self.resolveUpvalue(self.current, name);
        if (arg != -1) {
            getop = .GetUpvalue;
            setop = .SetUpvalue;
        } else {
            arg = self.identifierConstant(name);
            getop = .GetGlobal;
            setop = .SetGlobal;
        }
    }

    const op = @intToEnum(OpCode, arg);
    if (canAssign and self.match(.Equal)) {
        self.expression();
        self.emitBytes(setop, op);
    } else {
        self.emitBytes(getop, op);
    }
}

inline fn variable(self: *Self, canAssign: bool) void {
    self.namedVariable(&self.parser.previous, canAssign);
}

fn number(self: *Self) void {
    // TODO: Handle error properly
    const float = (std.fmt.parseFloat(f32, self.parser.previous.lexeme) catch {
        std.debug.panic("Could not parse as f32 '{s}'", .{self.parser.previous.lexeme});
    });
    self.emitConstant(Value.fromF32(float));
}

fn unary(self: *Self) void {
    const operatorKind = self.parser.previous.kind;
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

    const canAssign = @enumToInt(precedence) <= @enumToInt(Precedence.Assignment);
    self.prefix(self.parser.previous.kind, canAssign);

    while (@enumToInt(precedence) <= @enumToInt(getPrecedence(self.parser.current.kind))) {
        self.advance();
        self.infix(self.parser.previous.kind, canAssign);
    }

    if (canAssign and self.match(.Equal)) {
        self.@"error"("Invalid assignment target.");
    }
}

fn prefix(self: *Self, kind: TokenKind, canAssign: bool) void {
    switch (kind) {
        .LeftParen => self.groupedExpression(),
        .Bang, .Minus => self.unary(),

        .Nil, .True, .False => self.literal(),

        .String => self.string(),
        .Number => self.number(),

        .Identifier => self.variable(canAssign),

        // Should error instead
        else => unreachable,
    }
}

fn infix(self: *Self, kind: TokenKind, canAssign: bool) void {
    _ = canAssign;
    switch (kind) {
        .Plus, .Minus, .Star, .Slash => self.binary(),
        .LeftParen => self.call(),

        .BangEqual, .EqualEqual => self.binary(),
        .Greater, .GreaterEqual => self.binary(),
        .Less, .LessEqual => self.binary(),

        .And => self.@"and"(),
        .Or => self.@"or"(),
        // Should error instead
        else => unreachable,
    }
}

inline fn emitConstant(self: *Self, value: Value) void {
    self.emitBytes(.Constant, @intToEnum(OpCode, self.makeConstant(value)));
}

fn makeConstant(self: *Self, value: Value) u8 {
    // TODO: Handle error
    var constant = self.currentChunk().addConstant(value) catch {
        self.@"error"("Too many constants in one chunk.");
        return 0;
    };

    return constant;
}

inline fn errorAtCurrent(self: *Self, msg: []const u8) void {
    self.errorAt(&self.parser.current, msg);
}

inline fn @"error"(self: *Self, msg: []const u8) void {
    self.errorAt(&self.parser.previous, msg);
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
        self.parser.current = self.scanner.getToken();
        if (self.parser.current.kind != .Error) break;
        self.errorAtCurrent(self.parser.current.lexeme);
    }
}

inline fn check(self: *Self, kind: TokenKind) bool {
    return self.parser.current.kind == kind;
}

fn match(self: *Self, kind: TokenKind) bool {
    if (!self.check(kind)) return false;
    self.advance();
    return true;
}

inline fn identifierConstant(self: *Self, token: *Token) u8 {
    return self.makeConstant(Value.fromObject(&Object.ObjectString.copy(self.vm, token.lexeme).object));
}

fn identifiersEqual(a: *Token, b: *Token) bool {
    if (a.lexeme.len != b.lexeme.len) return false;
    return std.mem.eql(u8, a.lexeme, b.lexeme);
}

fn resolveLocal(self: *Self, compiler: *Compiler, name: *Token) i32 {
    if (compiler.count == 0) return -1;

    var i: usize = 0;
    while (i < compiler.count) : (i += 1) {
        var local = compiler.locals[compiler.count - 1 - i];
        if (identifiersEqual(name, &local.name)) {
            if (local.depth == -1) {
                self.@"error"("Cannot read local variable in its own initialiser.");
            }
            return @intCast(i32, i);
        }
    }
    return -1;
}

fn addUpvalue(self: *Self, compiler: *Compiler, index: u8, isLocal: bool) u32 {
    const upvalueCount = compiler.function.upvalueCount;

    for (0..upvalueCount) |idx| {
        const upvalue = &compiler.upvalues[idx];
        if (upvalue.index == index and upvalue.isLocal) {
            return @intCast(u32, idx);
        }
    }

    if (upvalueCount == 255) {
        self.@"error"("Too many closure variables in function.");
        return 0;
    }

    compiler.function.upvalueCount += 1;

    compiler.upvalues[upvalueCount].isLocal = isLocal;
    compiler.upvalues[upvalueCount].index = index;
    return upvalueCount;
}

fn resolveUpvalue(self: *Self, compiler: *Compiler, name: *Token) i32 {
    if (compiler.enclosing == null) return -1;

    const local = self.resolveLocal(compiler.enclosing.?, name);
    if (local != -1) {
        return @intCast(i32, self.addUpvalue(compiler, @intCast(u8, local), true));
    }

    // Resolve existing upvalue
    const upvalue = self.resolveUpvalue(compiler.enclosing.?, name);
    if (upvalue != -1) {
        return @intCast(i32, self.addUpvalue(compiler, @intCast(u8, upvalue), false));
    }

    return -1;
}

fn addLocal(self: *Self, name: *Token) void {
    if (self.current.count == std.math.maxInt(u8)) {
        self.@"error"("Too many variables in function.");
        return;
    }

    const local = &self.current.locals[@intCast(usize, self.current.count)];
    self.current.count += 1;

    local.name = name.*;
    local.depth = -1;
}

fn declareVariable(self: *Self) void {
    if (self.current.scopeDepth == 0) return;

    const name = &self.parser.previous;

    if (self.current.count > 0) {
        var i: usize = @intCast(usize, self.current.count) - 1;
        while (i >= 0) : (i -= 1) {
            const local = &self.current.locals[i];
            if (local.depth != -1 and local.depth < self.current.scopeDepth) {
                break;
            }

            if (identifiersEqual(name, &local.name)) {
                self.@"error"("Already a variable with this name in this scope.");
            }
        }
    }

    self.addLocal(name);
}

fn parseVariable(self: *Self, msg: []const u8) u8 {
    self.consume(.Identifier, msg);

    self.declareVariable();
    if (self.current.scopeDepth > 0) return 0;

    return self.identifierConstant(&self.parser.previous);
}

inline fn markInitialised(self: *Self) void {
    if (self.current.scopeDepth == 0) {
        return;
    }
    self.current.locals[@intCast(usize, self.current.count) - 1].depth = self.current.scopeDepth;
}

fn defineVariable(self: *Self, global: u8) void {
    if (self.current.scopeDepth > 0) {
        self.markInitialised();
        return;
    }

    self.emitBytes(.DefineGlobal, @intToEnum(OpCode, global));
}

inline fn expression(self: *Self) void {
    self.parsePrecendence(.Assignment);
}

fn block(self: *Self) void {
    while (!self.check(.RightBrace) and !self.check(.Eof)) {
        self.declaration();
    }
    self.consume(.RightBrace, "Expect '}' after block.");
}

fn expressionStatement(self: *Self) void {
    self.expression();
    self.consume(.Semicolon, "Expect ';' after expression.");
    self.emitByte(.Pop);
}

fn printStatement(self: *Self) void {
    self.expression();
    self.consume(.Semicolon, "Expect ';' after value.");
    self.emitByte(.Print);
}

inline fn declaration(self: *Self) void {
    if (self.match(.Fun)) {
        self.funDeclaration();
    } else if (self.match(.Var)) {
        self.varDeclaration();
    } else {
        self.statement();
    }

    if (self.parser.panicMode) self.sync();
}

fn emitLoop(self: *Self, loopStart: usize) void {
    self.emitByte(.Loop);

    const offset = self.currentChunk().code.items.len - loopStart + 2;
    if (offset > std.math.maxInt(u16)) {
        self.@"error"("Loop body is too large to jump.");
    }

    self.emitByteU8(@intCast(u8, (offset >> 8) & 0xff));
    self.emitByteU8(@intCast(u8, offset & 0xff));
}

fn @"and"(self: *Self) void {
    const endJump = self.emitJump(.JumpIfFalse);

    self.emitByte(.Pop);
    self.parsePrecendence(.And);

    self.patchJump(endJump);
}

fn @"or"(self: *Self) void {
    const elseJump = self.emitJump(.JumpIfFalse);
    const endJump = self.emitJump(.Jump);

    self.patchJump(elseJump);
    self.emitByte(.Pop);

    self.parsePrecendence(.Or);
    self.patchJump(endJump);
}

fn function(self: *Self, kind: FunctionKind) void {
    var compiler = Compiler.create(self.vm, kind, self.current);
    self.setCompiler(&compiler);

    self.beginScope();

    self.consume(.LeftParen, "Expect '(' after function name.");
    if (!self.check(.RightParen)) {
        while (true) {
            if (self.current.function.arity == 255) {
                self.errorAtCurrent("Can't have more than 255 parameters.");
            }
            self.current.function.arity += 1;

            const constant = self.parseVariable("Expect parameter name.");
            self.defineVariable(constant);

            if (!self.match(.Comma)) break;
        }
    }
    self.consume(.RightParen, "Expect ')' after parameter list.");

    self.consume(.LeftBrace, "Expect '{' before function body.");
    self.block();

    const func = self.end();
    self.emitByte(.Closure);
    self.emitByteU8(self.makeConstant(Value.fromObject(&func.object)));

    for (0..func.upvalueCount) |idx| {
        self.emitByteU8(if (compiler.upvalues[idx].isLocal) 1 else 0);
        self.emitByteU8(compiler.upvalues[idx].index);
    }
}

fn funDeclaration(self: *Self) void {
    const global = self.parseVariable("Expect function name.");
    self.markInitialised();
    self.function(.function);
    self.defineVariable(global);
}

fn varDeclaration(self: *Self) void {
    const global = self.parseVariable("Expect a variable name.");

    if (self.match(.Equal)) {
        self.expression();
    } else {
        self.emitByte(.Nil);
    }
    self.consume(.Semicolon, "Expect ';' after variable declaration.");

    self.defineVariable(global);
}

fn ifStatement(self: *Self) void {
    self.consume(.LeftParen, "Expect '(' after 'if'.");
    self.expression();
    self.consume(.RightParen, "Expect ')' after condition.");

    const thenJump = self.emitJump(.JumpIfFalse);
    self.emitByte(.Pop);
    self.statement();

    const elseJump = self.emitJump(.Jump);

    self.patchJump(thenJump);
    self.emitByte(.Pop);

    if (self.match(.Else)) {
        self.statement();
    }
    self.patchJump(elseJump);
}

fn returnStatement(self: *Self) void {
    if (self.current.functionKind == .script) {
        self.@"error"("Can't return from top-level code.");
    }

    if (self.match(.Semicolon)) {
        self.emitReturn();
        return;
    }

    self.expression();
    self.consume(.Semicolon, "Expect ';' after return value.");
    self.emitByte(.Return);
}

fn whileStatement(self: *Self) void {
    const loopStart = self.currentChunk().code.items.len;
    self.consume(.LeftParen, "Expect '(' after 'while'.");
    self.expression();
    self.consume(.RightParen, "Expect ')' after condition.");

    const exitJump = self.emitJump(.JumpIfFalse);
    self.emitByte(.Pop);
    self.statement();
    self.emitLoop(loopStart);

    self.patchJump(exitJump);
    self.emitByte(.Pop);
}

fn forStatement(self: *Self) void {
    self.beginScope();
    self.consume(.LeftParen, "Expect '(' after 'for'.");

    if (self.match(.Semicolon)) {
        // No initialiser
    } else if (self.match(.Var)) {
        self.varDeclaration();
    } else {
        self.expressionStatement();
    }

    var loopStart = self.currentChunk().code.items.len;
    var exitJump: i32 = -1;
    if (!self.match(.Semicolon)) {
        self.expression();
        self.consume(.Semicolon, "Expect ';' after loop condition.");

        exitJump = self.emitJump(.JumpIfFalse);
        self.emitByte(.Pop);
    }

    if (!self.match(.RightParen)) {
        const bodyJump = self.emitJump(.Jump);
        const incrementStart = self.currentChunk().code.items.len;

        self.expression();
        self.emitByte(.Pop);
        self.consume(.RightParen, "Expect ')' after for clauses.");

        self.emitLoop(loopStart);
        loopStart = incrementStart;
        self.patchJump(bodyJump);
    }

    self.statement();
    self.emitLoop(loopStart);

    if (exitJump != -1) {
        self.patchJump(exitJump);
        self.emitByte(.Pop); // Condition
    }

    self.endScope();
}

fn statement(self: *Self) void {
    switch (self.parser.current.kind) {
        .Print => {
            self.advance();
            self.printStatement();
        },
        .If => {
            self.advance();
            self.ifStatement();
        },
        .Return => {
            self.advance();
            self.returnStatement();
        },
        .While => {
            self.advance();
            self.whileStatement();
        },
        .For => {
            self.advance();
            self.forStatement();
        },
        .LeftBrace => {
            self.advance();

            self.beginScope();
            self.block();
            self.endScope();
        },
        else => self.expressionStatement(),
    }
}

fn sync(self: *Self) void {
    self.parser.panicMode = false;

    while (!self.check(.Eof)) {
        if (self.parser.previous.kind == .Semicolon) return;

        switch (self.parser.current.kind) {
            .Class, .Fun, .Var, .For, .If, .While, .Print, .Return => return,
            else => self.advance(),
        }
    }
}
