const std = @import("std");
const mem = std.mem;

pub const TokenKind = enum {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Plus,
    Minus,
    Star,
    Slash,
    Semicolon,
    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    // Literals.
    Identifier,
    String,
    Number,
    // Keywords.
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Error,
    Eof,
};

pub const Token = struct {
    kind: TokenKind,
    line: u32,
    lexeme: []const u8,
};

pub const Scanner = struct {
    start: []const u8,
    current: usize,
    line: u32,

    const Self = @This();

    pub fn init(source: []const u8) Self {
        return .{
            .start = source,
            .current = 0,
            .line = 1,
        };
    }

    pub fn getToken(self: *Self) Token {
        self.skipWhitespace();
        self.setStart();

        if (self.isAtEnd()) {
            return self.makeToken(.Eof);
        }

        var c = self.advance();

        if (isAlpha(c)) return self.identifier();
        if (isDigit(c)) return self.number();

        return switch (c) {
            '(' => self.makeToken(.LeftParen),
            ')' => self.makeToken(.RightParen),
            '{' => self.makeToken(.LeftBrace),
            '}' => self.makeToken(.RightBrace),
            ';' => self.makeToken(.Semicolon),
            ',' => self.makeToken(.Comma),
            '.' => self.makeToken(.Dot),
            '+' => self.makeToken(.Plus),
            '-' => self.makeToken(.Minus),
            '*' => self.makeToken(.Star),
            '/' => self.makeToken(.Slash),

            '!' => self.makeToken(if (self.match('=')) .BangEqual else .Bang),
            '=' => self.makeToken(if (self.match('=')) .EqualEqual else .Equal),
            '<' => self.makeToken(if (self.match('=')) .LessEqual else .Less),
            '>' => self.makeToken(if (self.match('=')) .GreaterEqual else .Greater),

            '"' => self.string(),
            else => self.errorToken("Unexpected character."),
        };
    }

    inline fn setStart(self: *Self) void {
        self.start = self.start[self.current..];
        self.current = 0;
    }

    fn skipWhitespace(self: *Self) void {
        while (true) {
            switch (self.peek()) {
                // Comments
                '/' => {
                    if (self.peekNext() == '/') {
                        _ = self.advance();
                        while (self.peek() != '\n' and !self.isAtEnd()) : (_ = self.advance()) {}
                    } else {
                        return;
                    }
                },
                // Space
                ' ', '\r', '\t' => _ = self.advance(),
                // Newlines
                '\n' => {
                    self.line += 1;
                    _ = self.advance();
                },
                else => return,
            }
        }
    }

    inline fn peek(self: *Self) u8 {
        if (self.isAtEnd()) return 0;
        return self.start[self.current];
    }

    inline fn peekNext(self: *Self) u8 {
        return if (self.current + 1 >= self.start.len)
            0
        else
            self.start[self.current + 1];
    }

    fn match(self: *Self, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.start[1] != expected) return false;
        self.current += 1;
        return true;
    }

    fn isAlpha(c: u8) bool {
        return (c >= 'a' and c <= 'z') or
            (c >= 'A' and c <= 'Z') or
            c == '_';
    }

    fn isDigit(c: u8) bool {
        return c >= '0' and c <= '9';
    }

    fn advance(self: *Self) u8 {
        defer self.current += 1;
        return self.start[self.current];
    }

    fn isAtEnd(self: *Self) bool {
        return self.current >= self.start.len;
    }

    fn makeToken(self: *Self, kind: TokenKind) Token {
        return .{
            .kind = kind,
            .lexeme = self.start[0..self.current],
            .line = self.line,
        };
    }

    fn errorToken(self: *Self, msg: []const u8) Token {
        return .{
            .kind = .Error,
            .lexeme = msg,
            .line = self.line,
        };
    }

    fn string(self: *Self) Token {
        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\n') {
                self.line += 1;
            }
            _ = self.advance();
        }

        if (self.isAtEnd()) {
            return self.errorToken("Unterminated string.");
        }

        // Closing quote
        _ = self.advance();
        return self.makeToken(.String);
    }

    fn number(self: *Self) Token {
        while (isDigit(self.peek())) : (_ = self.advance()) {}

        if (self.peek() == '.' and isDigit(self.peekNext())) {
            _ = self.advance();
            while (isDigit(self.peek())) : (_ = self.advance()) {}
        }

        return self.makeToken(.Number);
    }

    fn identifier(self: *Self) Token {
        while (isAlpha(self.peek()) or isDigit(self.peek())) : (_ = self.advance()) {}
        return self.makeToken(.Identifier);
    }

    fn identifierType(self: *Self) TokenKind {
        return switch (self.start[0]) {
            'a' => self.checkKeyword(1, "nd", .And),
            'c' => self.checkKeyword(1, "lass", .Class),
            'e' => self.checkKeyword(1, "lse", .Else),
            'f' => {
                return switch (self.start[1]) {
                    'a' => self.checkKeyword(2, "lse", .False),
                    'o' => self.checkKeyword(2, "r", .For),
                    'u' => self.checkKeyword(2, "n", .Fun),
                    else => .Identifier,
                };
            },
            'i' => self.checkKeyword(1, "f", .If),
            'n' => self.checkKeyword(1, "il", .Nil),
            'o' => self.checkKeyword(1, "r", .Or),
            'p' => self.checkKeyword(1, "rint", .Print),
            'r' => self.checkKeyword(1, "eturn", .Return),
            's' => self.checkKeyword(1, "uper", .Super),
            't' => {
                return switch (self.start[1]) {
                    'h' => self.checkKeyword(2, "is", .This),
                    'r' => self.checkKeyword(2, "ue", .True),
                    else => .Identifier,
                };
            },
            'v' => self.checkKeyword(1, "ar", .Var),
            'w' => self.checkKeyword(1, "hile", .While),
            else => .Identifier,
        };
    }

    fn checkKeyword(self: *Self, start: i32, rest: []const u8, kind: TokenKind) TokenKind {
        if (!self.current != start + rest.len) return .Identifier;

        if (mem.eql(u8, self.start[start..self.current], rest)) {
            return kind;
        }

        return .Identifier;
    }
};
