package parser

import "strconv"

type Token int

// TokenType
const (
	ILLEGAL Token = iota
	EOF
	COMMENT

	INT
	FLOAT
	STRING
	IDENT

	ADD
	SUB
	MUL
	DIV
	LSS
	GTR
	ASSIGN
	LPAREN
	LBRACK // [
	LBRACE // {
	COMMA  // ,
	RPAREN
	RBRACK // ]
	RBRACE
	SEMICOLON
	COLON // :
	PERIOD

	keyword_beg
	SYNTAX
	PACKAGE
	IMPORT
	OPTION
	SERVICE
	RPC
	RETURNS
	MESSAGE
	ENUM
	OPTIONAL
	REQUIRED
	REPEATED
	ONEOF
	MAP
	EXTENSIONS
	EXTEND
	RESERVED
	PUBLIC
	WEAK
	TO
	GROUP // pb2
	STREAM
	keyword_end
)

var tokens = [...]string{
	ILLEGAL: "ILLEGAL",
	EOF:     "EOF",
	COMMENT: "COMMENT",

	INT:    "INTERGER",
	FLOAT:  "FLOAT",
	STRING: "STRING",
	IDENT:  "IDENT",

	ADD:       "+",
	SUB:       "-",
	MUL:       "*",
	DIV:       "/",
	LSS:       "<",
	GTR:       ">",
	ASSIGN:    "=",
	LPAREN:    "(",
	LBRACK:    "[",
	LBRACE:    "{",
	COMMA:     ",",
	RPAREN:    ")",
	RBRACK:    "]",
	RBRACE:    "}",
	SEMICOLON: ";",
	COLON:     ":",
	PERIOD:    ".",

	SYNTAX:     "syntax",
	PACKAGE:    "package",
	IMPORT:     "import",
	OPTION:     "option",
	SERVICE:    "service",
	RPC:        "rpc",
	RETURNS:    "returns",
	MESSAGE:    "message",
	ENUM:       "enum",
	OPTIONAL:   "optional",
	REQUIRED:   "required",
	REPEATED:   "repeated",
	ONEOF:      "oneof",
	MAP:        "map",
	EXTENSIONS: "extensions",
	EXTEND:     "extend",
	RESERVED:   "reserved",
	PUBLIC:     "public",
	WEAK:       "weak",
	TO:         "to",
	GROUP:      "group",
	STREAM:     "stream",
}

func (tok Token) String() string {
	if 0 <= tok && tok < Token(len(tokens)) {
		return tokens[tok]
	}
	return "token(" + strconv.Itoa(int(tok)) + ")"
}

func (tok Token) IsKeyword() bool { return keyword_beg < tok && tok < keyword_end }

// reservedKeyWords 保留字
var reservedKeyWords map[string]Token

func init() {
	reservedKeyWords = make(map[string]Token)
	for i := SYNTAX; i < keyword_end; i++ {
		reservedKeyWords[tokens[i]] = i
	}
}

func Loopup(ident string) Token {
	if tok, ok := reservedKeyWords[ident]; ok {
		return tok
	}
	return IDENT
}

func IsKeyword(name string) bool {
	_, ok := reservedKeyWords[name]
	return ok
}
