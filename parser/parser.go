package parser

import (
	"fmt"
	"strconv"
	"strings"
)

var scalarType = map[string]bool{
	"double":   true,
	"float":    true,
	"int32":    true,
	"int64":    true,
	"uint32":   true,
	"uint64":   true,
	"sint32":   true,
	"sint64":   true,
	"fixed32":  true,
	"fixed64":  true,
	"sfixed32": true,
	"sfixed64": true,
	"bool":     true,
	"string":   true,
	"bytes":    true,
}

func IsScalar(t string) bool {
	return scalarType[t]
}

// Node ast
type Node interface {
	Pos() Pos
}

type Expr interface {
	Node
}

// https://developers.google.com/protocol-buffers/docs/reference/proto3-spec#message_definition

// File program
type File struct {
	Syntax   *Syntax
	Decls    []Expr
	Comments []*CommentGroup
}

func (x *File) Pos() Pos { return x.Syntax.Pos() }

// Lexical elements
type (
	Ident struct {
		NamePos Pos
		Name    string
	}
	// FullIdent = ident { "." ident }
	FullIdent struct {
		BeginPeriod Pos
		Parts       []*Ident
	}

	Integer struct {
		Num   Pos
		Value string
	}

	Float struct {
		Float Pos
		Value float64
	}

	String struct {
		String Pos
		Value  string
	}

	// MessageType = [ "." ] { ident "." } messageName
	MessageType struct {
		PeriodPos   Pos
		Idents      []*Ident
		MessageName *Ident
	}
)

func (x *Ident) Pos() Pos { return x.NamePos }
func (x *FullIdent) Pos() Pos {
	if x.BeginPeriod > NoPos {
		return x.BeginPeriod
	}
	return x.Parts[0].Pos()
}
func (x *Integer) Pos() Pos { return x.Num }
func (x *Float) Pos() Pos   { return x.Float }
func (x *String) Pos() Pos  { return x.String }
func (x *MessageType) Pos() Pos {
	if x.PeriodPos != NoPos {
		return x.PeriodPos
	}
	if len(x.Idents) > 0 {
		return x.Idents[0].Pos()
	}
	return x.MessageName.Pos()
}

type Syntax struct {
	Syntax Pos
	Name   *String
}

func (x *Syntax) Pos() Pos { return x.Syntax }

type Import struct {
	Import Pos
	Label  *Ident
	Name   *String
}

func (x *Import) Pos() Pos { return x.Import }

type Package struct {
	Package Pos
	Name    *FullIdent
}

func (x *Package) Pos() Pos { return x.Package }

type Option struct {
	Option Pos
	Name   *OptionName
	Value  Node
}

func (x *Option) Pos() Pos { return x.Option }

// OptionName = ( ident | "(" fullIdent ")" ) { "." ident }
type OptionName struct {
	NamePos      Pos
	Lparen       Pos
	Name         Expr
	Rparen       Pos
	PeriodIdents []*Ident
}

func (x *OptionName) Pos() Pos { return x.NamePos }

type Extensions struct {
	Extensions Pos
	Ranges     *Ranges
}

func (x *Extensions) Pos() Pos { return x.Extensions }

type Reserved struct {
	Value Expr
}

func (x *Reserved) Pos() Pos { return x.Value.Pos() }

type (
	Message struct {
		Message Pos
		Name    *Ident
		Fields  *FieldList

		Parent *Message
	}

	Enum struct {
		Enum   Pos
		Name   *Ident
		Fields *FieldList
	}

	Service struct {
		Service Pos
		Name    *Ident
		Fields  *FieldList
	}

	// Extend = "extend" messageType "{" {field | group | emptyStatement} "}"
	Extend struct {
		Extend      Pos
		MessageType *MessageType
		Fields      *FieldList
	}
)

func (x *Message) Pos() Pos { return x.Message }
func (x *Enum) Pos() Pos    { return x.Enum }
func (x *Service) Pos() Pos { return x.Service }
func (x *Extend) Pos() Pos  { return x.Extend }

// Fields
type (
	Field struct {
		TokPos  Pos
		Label   Token
		Type    Expr
		Name    *Ident
		Number  *Integer
		Options *FieldOptions
	}

	MapField struct {
		Map       Pos // position of "map" keyword
		KeyType   *Ident
		ValueType Expr
		Name      *Ident
		Number    *Integer
		Options   *FieldOptions
	}

	GroupField struct {
		Label  Token
		Group  Pos
		Name   *Ident
		Number *Integer
		Fields *FieldList
	}

	Oneof struct {
		Oneof  Pos
		Name   *Ident
		Fields *FieldList
	}
)

func (x *Field) Pos() Pos      { return x.TokPos }
func (x *MapField) Pos() Pos   { return x.Map }
func (x *GroupField) Pos() Pos { return x.Group }
func (x *Oneof) Pos() Pos      { return x.Oneof }

type (
	FieldList struct {
		Opening Pos
		List    []Expr
		Closing Pos
	}

	EnumField struct {
		Enum    Pos
		Name    *Ident
		TokPos  Pos
		ID      *Integer
		Options *FieldOption
	}
	FieldOptions struct {
		Opening Pos
		List    []*FieldOption
		Closing Pos
	}
	FieldOption struct {
		Name  *OptionName
		Value Expr
	}

	KeyValueExpr struct {
		Key   *Ident
		Value Expr
	}

	Rpc struct {
		Rpc            Pos
		Name           *Ident
		StreamRequest  Pos
		RequestType    *MessageType
		StreamResponse Pos
		ResponseType   *MessageType
		Body           *FieldList
	}

	Ranges struct {
		Ranges []*Range
	}
	Range struct {
		Lit *Integer
		To  Expr // Num or ID
	}

	StrFieldNames struct {
		Strings []*String
	}

	OneofField struct {
		Type    Expr
		Name    *Ident
		Number  *Integer
		Options *FieldOptions
	}
)

func (x *FieldOption) Pos() Pos   { return x.Name.NamePos }
func (x *FieldOptions) Pos() Pos  { return x.Opening }
func (x *FieldOptions) End() Pos  { return x.Closing }
func (x *Ranges) Pos() Pos        { return x.Ranges[0].Pos() }
func (x *Range) Pos() Pos         { return x.Lit.Pos() }
func (x *StrFieldNames) Pos() Pos { return x.Strings[0].Pos() }
func (x *Rpc) Pos() Pos           { return x.Rpc }
func (x *OneofField) Pos() Pos    { return x.Type.Pos() }
func (x *FieldList) Pos() Pos     { return x.Opening }
func (x *FieldList) End() Pos     { return x.Closing }
func (x *KeyValueExpr) Pos() Pos  { return x.Key.Pos() }
func (x *EnumField) Pos() Pos     { return x.Enum }

func (x *Message) FindNestedMessage(name string) *Message {
	for _, v := range x.Fields.List {
		switch t := v.(type) {
		case *Message:
			if t.Name.Name == name {
				return t
			}
		}
	}
	return nil
}

func (x *Message) GetName() string {
	name := x.Name.Name
	for t := x.Parent; t != nil; t = t.Parent {
		if name[0] >= 97 && name[0] <= 122 {
			name = t.Name.Name + strings.ToTitle(name)
		} else {
			name = t.Name.Name + "_" + name
		}
	}
	return name
}

type CommentGroup struct {
	List []*Comment
}

func (g *CommentGroup) Pos() Pos { return g.List[0].Pos() }
func (g *CommentGroup) End() Pos { return g.List[len(g.List)-1].End() }

type Comment struct {
	Slash Pos
	Text  string
}

func (c *Comment) Pos() Pos { return c.Slash }
func (c *Comment) End() Pos { return Pos(int(c.Slash) + len(c.Text) - 1) }

// Parser parse a proto file
type Parser struct {
	file  *FileInfo
	lexer *Lexer

	syntax *Syntax

	comments    []*CommentGroup
	lineComment *CommentGroup

	pos Pos
	tok Token
	lit string
}

// NewParser new
func NewParser(file *FileInfo, src []byte) *Parser {
	p := &Parser{
		file:  file,
		lexer: NewLexer(file, src),
	}
	p.init()
	return p
}

func (p *Parser) init() {
	p.next()
}

func (p *Parser) next() {
	p.next0()
	p.lineComment = nil
	for p.tok == COMMENT {
		p.lineComment = p.parseCommentGroup()
	}
}

func (p *Parser) next0() {
	p.pos, p.tok, p.lit = p.lexer.Next()
}

func (p *Parser) eat(tok Token) {
	if tok == p.tok {
		p.next()
	} else {
		panic(fmt.Errorf("expected tok[%v] but current tok is [%v]", tok, p.tok))
	}
}

func (p *Parser) parseCommentGroup() (comments *CommentGroup) {
	var list []*Comment
	for p.tok == COMMENT {
		list = append(list, &Comment{Slash: p.pos, Text: p.lit})
		if p.lexer.IsLineEnd() {
			p.next0()
			break
		}
		p.next0()
	}
	comments = &CommentGroup{List: list}
	p.comments = append(p.comments, comments)
	return
}

func (p *Parser) parseSyntax() *Syntax {
	pos := p.pos
	p.eat(SYNTAX)
	p.eat(ASSIGN)
	node := p.parseString()
	p.eat(SEMICOLON)
	return &Syntax{
		Syntax: pos,
		Name:   node,
	}
}

// ParseFile program
func (p *Parser) ParseFile() *File {
	p.syntax = p.parseSyntax()
	var decls []Expr
	var pack *Package
	for p.tok != EOF {
		switch p.tok {
		case PACKAGE:
			if pack != nil {
				panic("repeated package")
			}
			pack = p.parsePackage()
			decls = append(decls, pack)
		case IMPORT:
			decls = append(decls, p.parseImport())
		case OPTION:
			decls = append(decls, p.parseOption())
		case ENUM:
			decls = append(decls, p.parseEnum())
		case MESSAGE:
			decls = append(decls, p.parseMessage(nil))
		case SERVICE:
			decls = append(decls, p.parseService())
		case EXTEND:
			decls = append(decls, p.parseExtend())
		default:
			panic("parseFile not implement")
		}
	}
	return &File{Syntax: p.syntax, Decls: decls, Comments: p.comments}
}

func (p *Parser) parseImport() *Import {
	pos := p.pos
	p.eat(IMPORT)
	var label *Ident
	if p.tok == PUBLIC || p.tok == WEAK {
		label = p.parseIdent()
	}
	node := p.parseString()
	p.eat(SEMICOLON)
	return &Import{
		Import: pos,
		Name:   node,
		Label:  label,
	}
}

func (p *Parser) parsePackage() *Package {
	pos := p.pos
	p.eat(PACKAGE)
	n := p.parseFullIdent()
	p.eat(SEMICOLON)
	return &Package{Package: pos, Name: n}
}

func (p *Parser) parseOption() *Option {
	pos := p.pos
	p.eat(OPTION)
	name := p.parseOptionName()
	p.eat(ASSIGN)
	var node Node
	if p.tok == LBRACE {
		node = p.parseKeyValueArray()
		if p.tok == SEMICOLON {
			p.eat(SEMICOLON)
		}
	} else {
		node = p.parseConst()
		p.eat(SEMICOLON)
	}
	return &Option{Option: pos, Name: name, Value: node}
}

func (p *Parser) parseKeyValueArray() *FieldList {
	opening := p.pos
	p.eat(LBRACE)
	var list []Expr
	for p.tok != RBRACE {
		list = append(list, p.parseKeyValue())
	}
	closing := p.pos
	p.eat(RBRACE)
	return &FieldList{Opening: opening, List: list, Closing: closing}
}

func (p *Parser) parseKeyValue() *KeyValueExpr {
	key := p.parseIdent()
	var value Expr
	if p.tok == LBRACE {
		value = p.parseKeyValueArray()
	} else {
		p.eat(COLON)
		value = p.parseConst()
	}
	return &KeyValueExpr{
		Key:   key,
		Value: value,
	}
}

func (p *Parser) parseConst() Expr {
	switch p.tok {
	case IDENT:
		return p.parseIdent()
	case STRING:
		return p.parseString()
	default:
		if p.tok == SUB {
			if t, _ := p.lexer.PeekToken(); t == INT {
				return p.parseInteger()
			} else {
				return p.parseFloat()
			}
		}
		switch p.tok {
		case INT:
			return p.parseInteger()
		case FLOAT:
			return p.parseFloat()
		}
	}
	panic("not implement")
}

func (p *Parser) parseEnum() *Enum {
	e := &Enum{
		Enum: p.pos,
	}
	p.eat(ENUM)
	e.Name = p.parseIdent()
	opening := p.pos
	p.eat(LBRACE)
	var list []Expr
	for p.tok != RBRACE {
		switch p.tok {
		case IDENT:
			node := p.parseEnumField()
			list = append(list, node)
		case RESERVED:
			list = append(list, p.parseReserved())
		case OPTION:
			list = append(list, p.parseOption())
		default:
			panic("parseEnum not implement")
		}
	}
	closing := p.pos
	p.eat(RBRACE)
	e.Fields = &FieldList{
		Opening: opening,
		List:    list,
		Closing: closing,
	}
	return e
}

func (p *Parser) parseEnumField() *EnumField {
	pos := p.pos
	name := p.parseIdent()
	tokPos := p.pos
	p.eat(ASSIGN)
	id := p.parseInteger()
	var option *FieldOption
	if p.tok == LBRACK {
		p.eat(LBRACK)
		option = p.parseFieldOption()
		p.eat(RBRACK)
	}
	p.eat(SEMICOLON)
	return &EnumField{
		Enum:    pos,
		Name:    name,
		TokPos:  tokPos,
		ID:      id,
		Options: option,
	}
}

func (p *Parser) parseMessage(parent *Message) *Message {
	pos := p.pos
	p.eat(MESSAGE)
	name := p.parseIdent()
	opening := p.pos
	p.eat(LBRACE)
	m := &Message{
		Message: pos,
		Parent:  parent,
		Name:    name,
	}
	var list []Expr
	str := p.syntax.Name.Value[1 : len(p.syntax.Name.Value)-1]
	var pb2 = str == "proto2"
	for p.tok != RBRACE {
		var node Expr
		switch p.tok {
		case ENUM:
			node = p.parseEnum()
		case MESSAGE:
			node = p.parseMessage(m)
		case EXTENSIONS:
			if !pb2 {
				panic("proto2 not support extensions")
			}
			node = p.parseExtensions()
		case RESERVED:
			node = p.parseReserved()
		case MAP:
			node = p.parseMapField()
		case ONEOF:
			node = p.parseOneof()
		case EXTEND:
			node = p.parseExtend()
		case OPTION:
			node = p.parseOption()
		case REPEATED, OPTIONAL:
			if tok, _ := p.lexer.PeekToken(); tok == GROUP {
				if pb2 {
					node = p.parseGroupField()
				} else {
					panic("proto3 not support group")
				}
			} else {
				node = p.parseField()
			}
		case REQUIRED:
			if !pb2 {
				panic("proto3 not support required")
			}
			node = p.parseField()
		default:
			if pb2 {
				panic("proto2 need optional")
			}
			node = p.parseField()
		}
		list = append(list, node)
	}
	m.Fields = &FieldList{Opening: opening, List: list, Closing: p.pos}
	p.eat(RBRACE)
	return m
}

func (p *Parser) parseService() *Service {
	pos := p.pos
	p.eat(SERVICE)
	name := p.parseIdent()
	opening := p.pos
	p.eat(LBRACE)
	var fields []Expr
	for p.tok != RBRACE {
		switch p.tok {
		case RPC:
			fields = append(fields, p.parseRpc())
		case OPTION:
			fields = append(fields, p.parseOption())
		default:
			panic("not support")
		}
	}
	closing := p.pos
	p.eat(RBRACE)
	return &Service{
		Service: pos,
		Name:    name,
		Fields: &FieldList{
			Opening: opening,
			List:    fields,
			Closing: closing,
		},
	}
}

func (p *Parser) parseRpc() *Rpc {
	pos := p.pos
	p.eat(RPC)
	name := p.parseIdent()
	p.eat(LPAREN)
	var streamRequest, streamResponse Pos
	if p.tok == STREAM {
		streamRequest = p.pos
		p.eat(p.tok)
	}
	param := p.parseMessageType()
	p.eat(RPAREN)
	p.eat(RETURNS)
	p.eat(LPAREN)
	if p.tok == STREAM {
		streamResponse = p.pos
		p.eat(p.tok)
	}
	result := p.parseMessageType()
	p.eat(RPAREN)
	var body *FieldList
	if p.tok == LBRACE {
		body = p.parseRpcBody()
	} else {
		p.eat(SEMICOLON)
	}
	return &Rpc{
		Rpc:            pos,
		Name:           name,
		StreamRequest:  streamRequest,
		RequestType:    param,
		StreamResponse: streamResponse,
		ResponseType:   result,
		Body:           body,
	}
}

func (p *Parser) parseRpcBody() *FieldList {
	opening := p.pos
	p.eat(LBRACE)
	var options []Expr
	for p.tok == OPTION {
		options = append(options, p.parseOption())
	}
	closing := p.pos
	p.eat(RBRACE)
	return &FieldList{
		Opening: opening,
		List:    options,
		Closing: closing,
	}
}

func (p *Parser) parseExtend() *Extend {
	pos := p.pos
	p.eat(EXTEND)
	name := p.parseMessageType()
	opening := p.pos
	p.eat(LBRACE)
	var fields []Expr
	for p.tok != RBRACE {
		if t, _ := p.lexer.PeekToken(); t == GROUP {
			fields = append(fields, p.parseGroupField())
		} else {
			fields = append(fields, p.parseField())
		}
	}
	closing := p.pos
	p.eat(RBRACE)
	return &Extend{
		Extend:      pos,
		MessageType: name,
		Fields: &FieldList{
			Opening: opening,
			List:    fields,
			Closing: closing,
		},
	}
}

func (p *Parser) parseReserved() *Reserved {
	p.eat(RESERVED)
	var expr Expr
	if p.tok == INT || p.tok == SUB {
		expr = p.parseRanges()
	} else {
		expr = p.parseStrFieldNames()
	}
	p.eat(SEMICOLON)
	return &Reserved{
		Value: expr,
	}
}

func (p *Parser) parseExtensions() *Extensions {
	pos := p.pos
	p.eat(EXTENSIONS)
	ranges := p.parseRanges()
	p.eat(SEMICOLON)
	return &Extensions{
		Extensions: pos,
		Ranges:     ranges,
	}
}

func (p *Parser) parseField() *Field {
	pos := p.pos
	var tok Token
	if p.tok == OPTIONAL || p.tok == REQUIRED || p.tok == REPEATED {
		tok = p.tok
		p.eat(tok)
	}

	var typ = p.parseFieldType()
	name := p.parseIdent()
	p.eat(ASSIGN)
	id := p.parseInteger()
	options := p.parseFieldOptions()
	p.eat(SEMICOLON)
	return &Field{
		TokPos:  pos,
		Label:   tok,
		Type:    typ,
		Name:    name,
		Number:  id,
		Options: options,
	}
}

func (p *Parser) parseFieldType() Expr {
	if IsScalar(p.lit) {
		return p.parseIdent()
	} else {
		return p.parseMessageType()
	}
}

func (p *Parser) parseFieldOptions() *FieldOptions {
	if p.tok != LBRACK {
		return nil
	}
	opening := p.pos
	p.eat(LBRACK)
	var options []*FieldOption
	for p.tok != RBRACK {
		options = append(options, p.parseFieldOption())
		if p.tok != COMMA {
			break
		}
		p.eat(p.tok)
	}
	closing := p.pos
	p.eat(RBRACK)
	return &FieldOptions{Opening: opening, List: options, Closing: closing}
}

func (p *Parser) parseMapField() *MapField {
	pos := p.pos
	p.eat(MAP)
	p.eat(LSS)
	keyType := p.parseIdent()
	p.eat(COMMA)
	valueType := p.parseFieldType()
	p.eat(GTR)
	name := p.parseIdent()
	p.eat(ASSIGN)
	id := p.parseInteger()
	var options = p.parseFieldOptions()
	p.eat(SEMICOLON)
	return &MapField{
		Map:       pos,
		KeyType:   keyType,
		ValueType: valueType,
		Name:      name,
		Number:    id,
		Options:   options,
	}
}

func (p *Parser) parseOneof() *Oneof {
	pos := p.pos
	p.eat(ONEOF)
	name := p.parseIdent()
	opening := p.pos
	p.eat(LBRACE)
	var fields []Expr
	for p.tok != RBRACE {
		if p.tok == OPTION {
			fields = append(fields, p.parseOption())
		} else {
			fields = append(fields, p.parseOneofField())
		}
	}
	closing := p.pos
	p.eat(RBRACE)
	return &Oneof{
		Oneof: pos,
		Name:  name,
		Fields: &FieldList{
			Opening: opening,
			List:    fields,
			Closing: closing,
		},
	}
}

func (p *Parser) parseOneofField() *OneofField {
	typ := p.parseIdent()
	name := p.parseIdent()
	p.eat(ASSIGN)
	number := p.parseInteger()
	options := p.parseFieldOptions()
	p.eat(SEMICOLON)
	return &OneofField{
		Type:    typ,
		Name:    name,
		Number:  number,
		Options: options,
	}
}

func (p *Parser) parseFieldOption() *FieldOption {
	var name = p.parseOptionName()
	p.eat(ASSIGN)
	var value Expr
	if p.tok == LBRACE {
		value = p.parseKeyValueArray()
	} else {
		value = p.parseConst()
	}
	return &FieldOption{
		Name:  name,
		Value: value,
	}
}

func (p *Parser) parseIdent() *Ident {
	pos := p.pos
	name := p.lit
	p.eat(p.tok)
	return &Ident{NamePos: pos, Name: name}
}

func (p *Parser) parseInteger() *Integer {
	pos := p.pos
	pre := ""
	if p.tok == SUB {
		p.eat(p.tok)
		pre = "-"
	}
	lit := p.lit
	p.eat(INT)
	return &Integer{Num: pos, Value: pre + lit}
}

func (p *Parser) parseFloat() *Float {
	pos := p.pos
	pre := ""
	if p.tok == SUB {
		p.eat(p.tok)
		pre = "-"
	}
	lit := p.lit
	p.eat(FLOAT)
	v, err := strconv.ParseFloat(pre+lit, 64)
	if err != nil {
		panic(err)
	}
	return &Float{Float: pos, Value: v}
}

func (p *Parser) parseString() *String {
	pos := p.pos
	t := p.lit
	p.eat(STRING)
	return &String{
		String: pos,
		Value:  t,
	}
}

func (p *Parser) parseOptionName() *OptionName {
	pos := p.pos
	var name Expr
	var idents []*Ident
	opening, closing := NoPos, NoPos
	switch p.tok {
	case IDENT:
		name = p.parseIdent()
	case LPAREN:
		opening = p.pos
		p.eat(LPAREN)
		name = p.parseFullIdent()
		closing = p.pos
		p.eat(RPAREN)
	}
	for p.tok == PERIOD {
		p.eat(PERIOD)
		idents = append(idents, p.parseIdent())
	}
	return &OptionName{
		NamePos:      pos,
		Lparen:       opening,
		Name:         name,
		Rparen:       closing,
		PeriodIdents: idents,
	}
}

func (p *Parser) parseFullIdent() *FullIdent {
	var beginPeriodPos Pos
	if p.tok == PERIOD {
		beginPeriodPos = p.pos
		p.eat(PERIOD)
	}
	var idents []*Ident
	for {
		idents = append(idents, p.parseIdent())
		if p.tok != PERIOD {
			break
		}
		p.eat(PERIOD)
	}
	return &FullIdent{
		BeginPeriod: beginPeriodPos,
		Parts:       idents,
	}
}

func (p *Parser) parseMessageType() *MessageType {
	var periodPos Pos
	if p.tok == PERIOD {
		periodPos = p.pos
		p.eat(p.tok)
	}
	var idents []*Ident
	var messageName *Ident
	for p.tok == IDENT {
		messageName = p.parseIdent()
		if p.tok == PERIOD {
			idents = append(idents, messageName)
			p.eat(p.tok)
		} else {
			break
		}
	}
	return &MessageType{
		PeriodPos:   periodPos,
		Idents:      idents,
		MessageName: messageName,
	}
}

func (p *Parser) parseRanges() *Ranges {
	var r []*Range
	for {
		r = append(r, p.parseRange())
		if p.tok == COMMA {
			p.eat(COMMA)
		} else {
			break
		}
	}
	return &Ranges{Ranges: r}
}

func (p *Parser) parseRange() *Range {
	lit := p.parseInteger()
	var toLit Expr
	if p.tok == TO {
		p.eat(TO)
		switch p.tok {
		case INT, SUB:
			toLit = p.parseInteger()
		case STRING:
			toLit = p.parseString()
		case IDENT:
			toLit = p.parseIdent()
		default:
			panic("not support")
		}
	}
	return &Range{Lit: lit, To: toLit}
}

func (p *Parser) parseStrFieldNames() *StrFieldNames {
	var s []*String
	for {
		s = append(s, p.parseString())
		if p.tok != COMMA {
			break
		}
		p.eat(COMMA)
	}
	return &StrFieldNames{Strings: s}
}

func (p *Parser) parseGroupField() *GroupField {
	pos := p.pos
	label := p.tok
	p.eat(p.tok)
	p.eat(GROUP)
	name := p.parseIdent()
	var num *Integer
	if p.tok == ASSIGN {
		p.eat(ASSIGN)
		num = p.parseInteger()
	}
	opening := p.pos
	p.eat(LBRACE)
	var fields []Expr
	for p.tok != RBRACE {
		fields = append(fields, p.parseField())
	}
	closing := p.pos
	p.eat(RBRACE)
	return &GroupField{
		Label:  label,
		Group:  pos,
		Name:   name,
		Number: num,
		Fields: &FieldList{
			Opening: opening,
			List:    fields,
			Closing: closing,
		},
	}
}