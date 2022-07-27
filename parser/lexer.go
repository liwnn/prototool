package parser

import (
	"strconv"
	"unicode/utf8"
)

// Lexer lexer
type Lexer struct {
	file *FileInfo
	src  []byte
	pos  int
}

// NewLexer new lexer
func NewLexer(file *FileInfo, src []byte) *Lexer {
	l := &Lexer{
		file: file,
		src:  src,
		pos:  0,
	}
	return l
}

func (l *Lexer) next() (rune, int) {
	r, w := rune(l.src[l.pos]), 1
	switch {
	case r == 0:
	case r >= utf8.RuneSelf:
		r, w = utf8.DecodeRune(l.src[l.pos:])
	}
	return r, w
}

func (l *Lexer) Next() (pos Pos, tok Token, lit string) {
	l.skipWhiteSpace()
	if l.pos >= len(l.src) {
		return Pos(len(l.src)), EOF, ""
	}
	ch, _ := l.next()
	pos = Pos(l.pos)
	switch {
	case ch >= 'A' && ch <= 'Z' || ch >= 'a' && ch <= 'z' || ch == '_':
		lit = l.id()
		tok = Loopup(lit)
	case ch >= '0' && ch <= '9':
		tok, lit = l.number()
	default:
		l.pos++
		switch ch {
		case '"', '\'':
			tok = STRING
			lit = l.scanString()
		case '+':
			tok = ADD
		case '-':
			tok = SUB
		case '*':
			tok = MUL
		case '.':
			tok = PERIOD
		case '/':
			if l.src[l.pos] == '/' {
				var prevPos = l.pos - 1
				for l.pos++; l.pos < len(l.src) && l.src[l.pos] != '\n'; l.pos++ {
				}
				tok = COMMENT
				lit = string(l.src[prevPos:l.pos])
			} else if l.src[l.pos] == '*' {
				var prevPos = l.pos - 1
				for l.pos++; !(l.src[l.pos] == '*' && l.src[l.pos+1] == '/'); l.pos++ {
					if l.src[l.pos] == '\n' {
						l.file.AddLine(l.pos)
					}
				}
				l.pos += 2
				tok = COMMENT
				lit = string(l.src[prevPos:l.pos])
			} else {
				tok = DIV
			}
		case '(':
			tok = LPAREN
		case ')':
			tok = RPAREN
		case ':':
			tok = COLON
		case '=':
			tok = ASSIGN
		case ';':
			tok = SEMICOLON
		case ',':
			tok = COMMA
		case '{':
			tok = LBRACE
		case '}':
			tok = RBRACE
		case '[':
			tok = LBRACK
		case ']':
			tok = RBRACK
		case '<':
			tok = LSS
		case '>':
			tok = GTR
		default:
			panic("getToken:" + strconv.Itoa(int(ch)))
		}
	}
	return
}

func (l *Lexer) PeekToken() (token Token, lit string) {
	oldPos := l.pos
	for {
		_, token, lit = l.Next()
		if token != COMMENT {
			break
		}
	}
	l.pos = oldPos
	return token, lit
}

func (l *Lexer) IsLineEnd() bool {
	return l.pos >= len(l.src) || l.src[l.pos] == '\n'
}

func (l *Lexer) skipWhiteSpace() {
	if l.pos >= len(l.src) {
		return
	}
	ch := l.src[l.pos]
	for ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r' {
		if ch == '\n' {
			l.file.AddLine(l.pos)
		}
		l.pos++
		if l.pos >= len(l.src) {
			return
		}
		ch = l.src[l.pos]
	}
}

// identifiers and reserved keywords
func (l *Lexer) id() string {
	begin := l.pos
	for l.pos++; l.pos < len(l.src) && (l.src[l.pos] >= 'A' && l.src[l.pos] <= 'Z' ||
		l.src[l.pos] >= 'a' && l.src[l.pos] <= 'z') || l.src[l.pos] >= '0' && l.src[l.pos] <= '9' ||
		l.src[l.pos] == '_'; l.pos++ {
	}
	return string(l.src[begin:l.pos])
}

func (l *Lexer) number() (Token, string) {
	begin := l.pos
	if l.src[l.pos] == '0' && l.src[l.pos+1] == 'x' {
		for l.pos = l.pos + 2; l.pos < len(l.src); l.pos++ {
			if ch := l.src[l.pos]; ch >= '0' && ch <= '9' || ch >= 'a' && ch <= 'z' || ch >= 'A' && ch <= 'Z' {

			} else {
				break
			}
		}
		return INT, string(l.src[begin:l.pos])
	}

LOOP:
	for l.pos++; l.pos < len(l.src); l.pos++ {
		ch := l.src[l.pos]
		switch {
		case ch >= '0' && ch <= '9':
		case ch == 'e' || ch == 'E':
			if l.src[l.pos+1] == '-' {
				l.pos++
			}
		default:
			break LOOP
		}
	}
	if l.pos < len(l.src) && l.src[l.pos] == '.' {
		for l.pos++; l.pos < len(l.src) && l.src[l.pos] >= '0' && l.src[l.pos] <= '9'; l.pos++ {
		}
		return FLOAT, string(l.src[begin:l.pos])
	}
	return INT, string(l.src[begin:l.pos])
}

func (l *Lexer) scanString() string {
	begin := l.pos - 1
	endChar := l.src[begin]
	for ; l.pos < len(l.src); l.pos++ {
		if l.src[l.pos] == '\\' { // ( "a" | "b" | "f" | "n" | "r" | "t" | "v" | '\' | "'" | '"' )
			l.pos++
			continue
		}
		if l.src[l.pos] != endChar {
			continue
		}
		break
	}
	l.pos++
	return string(l.src[begin:l.pos])
}
