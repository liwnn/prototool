package printer

import (
	"io"
	"strings"
	"text/tabwriter"
	"unicode"

	"github.com/liwnn/prototool/parser"
)

type whiteSpace byte

const (
	ignore   = whiteSpace(0)
	blank    = whiteSpace(' ')
	vtab     = whiteSpace('\v')
	formfeed = whiteSpace('\f')
	newline  = whiteSpace('\n')
	indent   = whiteSpace('>')
	unindent = whiteSpace('<')
)

const (
	maxNewlines = 2
	infinity    = 1 << 30
)

type printer struct {
	// config
	Config
	fset *parser.FileSet

	// Current state
	output  []byte
	indent  int
	wsbuf   []whiteSpace
	lastTok parser.Token

	// parser.Positions
	pos  parser.Position
	last parser.Position
	out  parser.Position

	// Comments
	comments      []*parser.CommentGroup
	comment       *parser.CommentGroup
	cindex        int
	commentOffset int
}

func newPrinter(cfg *Config, fset *parser.FileSet) *printer {
	return &printer{
		Config: *cfg,
		fset:   fset,
		wsbuf:  make([]whiteSpace, 0, 16),

		pos: parser.Position{Column: 1},
		out: parser.Position{Column: 1},
	}
}

func (p *printer) commentBefore(next parser.Position) bool {
	return p.commentOffset < next.Offset
}

func (p *printer) nextComment() {
	for p.cindex < len(p.comments) {
		c := p.comments[p.cindex]
		p.cindex++
		if list := c.List; len(list) > 0 {
			p.comment = c
			p.commentOffset = p.posFor(list[0].Pos()).Offset
			return
		}
	}
	// no more comments
	p.commentOffset = infinity
}

func (p *printer) commentsHaveNewline(list []*parser.Comment) bool {
	// len(list) > 0
	line := p.lineFor(list[0].Pos())
	for i, c := range list {
		if i > 0 && p.lineFor(list[i].Pos()) != line {
			// not all comments on the same line
			return true
		}
		if t := c.Text; len(t) >= 2 && (t[1] == '/' || strings.Contains(t, "\n")) {
			return true
		}
	}
	_ = line
	return false
}

func (p *printer) print(args ...interface{}) {
	for _, arg := range args {
		var data string
		var isLit bool
		switch x := arg.(type) {
		case parser.Pos:
			p.pos = p.posFor(x)
			continue
		case parser.Token:
			s := x.String()
			data = s
			p.lastTok = x
		case whiteSpace:
			i := len(p.wsbuf)
			if i == cap(p.wsbuf) {
				p.writeWhitespace(i)
				i = 0
			}
			p.wsbuf = p.wsbuf[0 : i+1]
			p.wsbuf[i] = x
			p.lastTok = parser.ILLEGAL
			continue
		case *parser.String:
			data = x.Value
			p.lastTok = parser.STRING
		case *parser.Ident:
			data = x.Name
			p.lastTok = parser.IDENT
		case *parser.Integer:
			data = x.Value
			p.lastTok = parser.INT
		case *parser.Float:
			data = x.Value
			p.lastTok = parser.FLOAT
		}
		next := p.pos
		_, _ = p.flush(next, p.lastTok)
		n := next.Line - p.pos.Line
		if n > 0 {
			ch := byte('\n')
			p.writeByte(ch, n)
		}

		p.writeString(next, data, isLit)
	}
}

func (p *printer) flush(next parser.Position, tok parser.Token) (wroteNewline, droppedFF bool) {
	if p.commentBefore(next) {
		wroteNewline, droppedFF = p.intersperseComments(next, tok)
	} else {
		p.writeWhitespace(len(p.wsbuf))
	}
	return
}

func (p *printer) intersperseComments(next parser.Position, tok parser.Token) (wroteNewline, droppedFF bool) {
	var last *parser.Comment
	for p.commentBefore(next) {
		for _, c := range p.comment.List {
			p.writeCommentPrefix(p.posFor(c.Pos()), next, last, tok)
			p.writeComment(c)
			last = c
		}
		p.nextComment()
	}
	needsLinebreak := false
	if last != nil {
		if p.lineFor(last.Pos()) == next.Line {
			p.writeByte(' ', 1)
		}
		if last.Text[1] == '/' || tok == parser.EOF || tok == parser.RBRACE {
			needsLinebreak = true
		}
	}
	return p.writeCommentSuffix(needsLinebreak)
}

func (p *printer) writeCommentPrefix(pos, next parser.Position, prev *parser.Comment, tok parser.Token) {
	if pos.Line == p.last.Line && (prev == nil || prev.Text[1] != '/') {
		hasSep := false
		if prev == nil {
			j := 0
			for i, ch := range p.wsbuf {
				switch ch {
				case blank:
					p.wsbuf[i] = ignore
					continue
				case vtab:
					hasSep = true
					continue
				case indent:
					continue
				}
				j = i
				break
			}
			p.writeWhitespace(j)
		}
		if !hasSep {
			sep := byte(' ')
			if p.CommentAlign {
				sep = byte('\t')
			}
			if pos.Line == next.Line {
				sep = byte(' ')
			}
			p.writeByte(sep, 1)
		}
	} else {
		droppedLinebreak := false
		j := 0
		for i, ch := range p.wsbuf {
			switch ch {
			case blank:
				p.wsbuf[i] = ignore
				continue
			case indent:
				continue
			case newline, formfeed:
				p.wsbuf[i] = ignore
				droppedLinebreak = prev == nil
			}
			j = i
			break
		}
		p.writeWhitespace(j)
		n := 0
		if p.last.Line > 0 {
			n = pos.Line - p.last.Line
			if n < 0 {
				n = 0
			}
		}
		if p.indent == 0 && droppedLinebreak {
			n++
		}

		if n == 0 && prev != nil && prev.Text[1] == '/' {
			n = 1
		}
		if n > 0 {
			p.writeByte('\f', nlimit(n))
		}
	}
}

func nlimit(n int) int {
	if n > maxNewlines {
		n = maxNewlines
	}
	return n
}

func (p *printer) writeCommentSuffix(needsLinebreak bool) (wroteNewline, droppedFF bool) {
	for i, ch := range p.wsbuf {
		switch ch {
		case newline, formfeed:
			if needsLinebreak {
				needsLinebreak = false
				wroteNewline = true
			} else {
				p.wsbuf[i] = ignore
			}
		}
	}
	p.writeWhitespace(len(p.wsbuf))
	if needsLinebreak {
		p.writeByte('\n', 1)
		wroteNewline = true
	}
	return
}

func (p *printer) writeByte(ch byte, n int) {
	if p.out.Column == 1 {
		p.writeIndent()
	}

	for i := 0; i < n; i++ {
		p.output = append(p.output, ch)
	}

	p.pos.Offset += n

	if ch == '\n' || ch == '\f' {
		p.pos.Line += n
		p.out.Line += n
		p.pos.Column = 1
		p.out.Column = 1
		return
	}
	p.pos.Column += n
	p.out.Column += n
}

func (p *printer) writeWhitespace(n int) {
	for i := 0; i < n; i++ {
		switch ch := p.wsbuf[i]; ch {
		case ignore:
		case indent:
			p.indent++
		case unindent:
			p.indent--
		default:
			p.writeByte(byte(ch), 1)
		}
	}
	l := copy(p.wsbuf, p.wsbuf[n:])
	p.wsbuf = p.wsbuf[:l]
}

func (p *printer) writeComment(comment *parser.Comment) {
	text := comment.Text
	pos := p.posFor(comment.Pos())
	p.writeString(pos, trimRight(text), true)
}

func (p *printer) posFor(pos parser.Pos) parser.Position {
	// not used frequently enough to cache entire token.parser.Position
	return p.fset.PositionFor(pos)
}

func trimRight(s string) string {
	return strings.TrimRightFunc(s, unicode.IsSpace)
}

func (p *printer) writeString(pos parser.Position, s string, isLit bool) {
	if p.out.Column == 1 {
		p.writeIndent()
	}
	p.pos = pos

	if isLit {
		p.output = append(p.output, tabwriter.Escape)
	}

	p.output = append(p.output, s...)

	nlines := 0
	var li int
	for i := 0; i < len(s); i++ {
		if ch := s[i]; ch == '\n' || ch == '\f' {
			nlines++
			li = i
		}
	}

	p.pos.Offset += len(s)
	if nlines > 0 {
		p.pos.Line += nlines
		p.out.Line += nlines
		c := len(s) - li
		p.pos.Column = c
		p.out.Column = c
	} else {
		p.pos.Column += len(s)
		p.out.Column += len(s)
	}
	if isLit {
		p.output = append(p.output, tabwriter.Escape)
	}
	p.last = p.pos
}

func (p *printer) writeIndent() {
	n := p.indent
	for i := 0; i < n; i++ {
		p.output = append(p.output, p.IndentBytes...)
	}

	p.pos.Offset += n
	p.pos.Column += n
	p.out.Column += n
}

func (p *printer) printNode(node parser.Node) {
	if n, ok := node.(*parser.File); ok {
		p.comments = n.Comments
	}

	p.nextComment()

	switch n := node.(type) {
	case *parser.File:
		p.file(n)
	}
}

func (p *printer) expr(expr parser.Expr) {
	switch x := expr.(type) {
	case *parser.Import:
		p.print(x.Pos(), parser.IMPORT, blank)
		if x.Label != nil {
			p.print(x.Label.Pos(), x.Label, blank)
		}
		p.expr(x.Name)
		p.print(parser.SEMICOLON)
	case *parser.OptionName:
		if x.Lparen > 0 {
			p.print(x.Lparen, parser.LPAREN)
		}
		p.expr(x.Name)
		if x.Rparen > 0 {
			p.print(x.Rparen, parser.RPAREN)
		}
		for _, v := range x.PeriodIdents {
			p.print(parser.PERIOD)
			p.expr(v)
		}
	case *parser.Option:
		p.print(x.Pos(), parser.OPTION, blank)
		p.expr(x.Name)
		p.print(blank, parser.ASSIGN, blank)
		p.expr(x.Value)
		p.print(parser.SEMICOLON)
	case *parser.String:
		p.print(x.Pos())
		p.print(x)
	case *parser.Ident:
		p.print(x.Pos(), x)
	case *parser.Integer:
		p.print(x.Pos(), x)
	case *parser.Float:
		p.print(x.Pos(), x)
	case *parser.Package:
		p.print(x.Pos(), parser.PACKAGE, blank)
		p.expr(x.Name)
		p.print(parser.SEMICOLON)
	case *parser.Message:
		p.print(x.Pos(), parser.MESSAGE, blank)
		p.expr(x.Name)
		p.print(blank)
		p.fieldList(x.Fields)
	case *parser.Enum:
		p.print(x.Pos(), parser.ENUM, blank)
		p.expr(x.Name)
		p.print(blank)
		p.fieldList(x.Fields)
	case *parser.Extend:
		p.print(x.Pos(), parser.EXTEND, blank)
		p.expr(x.MessageType)
		p.print(blank)
		p.fieldList(x.Fields)
	case *parser.Ranges:
		for i, r := range x.Ranges {
			if i != 0 {
				p.print(parser.COMMA, blank)
			}
			p.expr(r)
		}
	case *parser.Range:
		p.expr(x.Lit)
		if x.To != nil {
			p.print(blank, parser.TO, blank)
			p.expr(x.To)
		}
	case *parser.StrFieldNames:
		for i, s := range x.Strings {
			if i != 0 {
				p.print(parser.COMMA, blank)
			}
			p.expr(s)
		}
	case *parser.MultiLineString:
		var lastPos = p.out.Column - p.indent
		for i, v := range x.Strings {
			if i > 0 {
				p.linebreak(p.lineFor(v.Pos()), 1)
				p.flush(p.posFor(v.Pos()), parser.Token(parser.ILLEGAL))
				for i := 0; i < int(lastPos); i++ {
					p.print(blank)
				}
			}
			p.expr(v)
		}
	case *parser.Array:
		p.print(x.Opening, parser.LBRACK)
		for i, v := range x.List {
			if i != 0 {
				if i-1 < len(x.Sep) {
					p.print(x.Sep[i-1], blank)
				} else {
					p.print(newline)
				}
			}
			p.expr(v)
		}
		p.print(x.Closing, parser.RBRACK)
	case *parser.Service:
		p.print(x.Pos(), parser.SERVICE, blank)
		p.expr(x.Name)
		p.print(blank)
		p.fieldList(x.Fields)
	case *parser.MessageType:
		if x.PeriodPos > 0 {
			p.print(x.PeriodPos, parser.PERIOD)
		}
		if len(x.Idents) > 0 {
			for _, ident := range x.Idents {
				p.expr(ident)
				p.print(parser.PERIOD)
			}
		}
		p.expr(x.MessageName)
	case *parser.FullIdent:
		if x.BeginPeriod > 0 {
			p.print(x.BeginPeriod, parser.PERIOD)
		}
		p.expr(x.Parts[0])
		for _, ident := range x.Parts[1:] {
			p.print(parser.PERIOD)
			p.expr(ident)
		}
	case *parser.FieldList:
		p.fieldList(x)
	case *parser.KeyValueExpr:
		p.expr(x.Key)
		if x.ColonPos > 0 {
			p.print(x.ColonPos, parser.COLON)
		}
		p.print(blank)
		p.expr(x.Value)
		if x.CommaPos > 0 {
			p.print(x.CommaPos, parser.COMMA)
		}
	case *parser.KeyOption:
		p.print(x.Pos(), parser.LBRACK)
		for i, v := range x.Name {
			if i != 0 {
				p.print(parser.DIV)
			}
			p.expr(v)
		}
		p.print(x.End(), parser.RBRACK)
	default:
		panic("unhandled expr")
	}
}

func (p *printer) fieldList(fields *parser.FieldList) {
	hasComments := p.commentBefore(p.posFor(fields.Closing))
	srcIsOneLine := p.lineFor(fields.Pos()) == p.lineFor(fields.Closing)
	if /*!hasComments &&*/ srcIsOneLine {
		if len(fields.List) == 0 {
			p.print(fields.Pos(), fields.OpenTok, fields.End(), fields.CloseTok)
			return
		} else if len(fields.List) <= 2 {
			p.print(fields.Pos(), fields.OpenTok)
			for i, l := range fields.List {
				p.printField(l, blank, i == len(fields.List)-1, true)
			}
			p.print(fields.End(), fields.CloseTok)
			return
		}
	}
	p.print(fields.Pos(), fields.OpenTok, indent)
	if hasComments || len(fields.List) > 0 {
		p.print(formfeed)
	}
	sep := blank
	if p.FieldAlign && len(fields.List) > 1 {
		sep = vtab
	}
	for i, f := range fields.List {
		if i > 0 {
			p.linebreak(p.lineFor(f.Pos()), 1)
		}
		last := i == len(fields.List)-1
		p.printField(f, sep, last, false)
	}
	p.print(unindent, formfeed, fields.End(), fields.CloseTok)
}

func (p *printer) printField(f parser.Expr, sep whiteSpace, last bool, srcIsOneLine bool) {
	switch t := f.(type) {
	case *parser.Message, *parser.Option, *parser.Enum:
		p.expr(t)
	case *parser.Field:
		if t.Label != parser.ILLEGAL {
			p.print(t.Pos(), t.Label, blank)
		}
		p.expr(t.Type)
		p.print(sep)

		p.expr(t.Name)
		p.print(sep, t.AssignPos, parser.ASSIGN, sep)
		p.expr(t.Number)
		if t.Options != nil {
			p.print(blank)
			p.expr(t.Options)
		}
		p.print(parser.SEMICOLON)
	case *parser.MapField:
		p.print(t.Pos(), parser.MAP, parser.LSS, t.KeyType, parser.COMMA, blank)
		p.expr(t.ValueType)
		p.print(parser.GTR, sep)

		p.expr(t.Name)
		p.print(sep, parser.ASSIGN, sep)
		p.expr(t.Number)
		if t.Options != nil {
			p.print(blank)
			p.expr(t.Options)
		}
		p.print(parser.SEMICOLON)
	case *parser.GroupField:
		if t.Label != nil {
			p.expr(t.Label)
			p.print(blank)
		}
		p.print(t.Pos(), parser.GROUP, blank)
		p.expr(t.Name)
		p.print(blank)
		if t.Number != nil {
			p.print(parser.ASSIGN, blank, t.Number, blank)
		}
		if t.Options != nil {
			p.expr(t.Options)
		}
		p.fieldList(t.Fields)
	case *parser.EnumField:
		p.expr(t.Name)
		p.print(sep, t.TokPos, parser.ASSIGN, sep)
		p.expr(t.ID)
		if t.Options != nil {
			p.print(blank)
			p.expr(t.Options)
		}
		p.print(parser.SEMICOLON)
	case *parser.Reserved:
		p.print(t.Pos(), parser.RESERVED, blank)
		p.expr(t.Value)
		p.print(parser.SEMICOLON)
	case *parser.Rpc:
		p.print(t.Pos(), parser.RPC, blank)
		p.expr(t.Name)
		p.print(blank, t.LparenRequest, parser.LPAREN)
		if t.StreamRequest > 0 {
			p.print(t.StreamRequest, parser.STREAM, blank)
		}
		p.expr(t.RequestType)
		p.print(t.RparenRequest, parser.RPAREN, blank)
		p.print(t.Return, parser.RETURNS, blank, t.LparenResponse, parser.LPAREN)
		if t.StreamResponse > 0 {
			p.print(t.StreamResponse, parser.STREAM, blank)
		}
		p.expr(t.ResponseType)
		p.print(t.RparenResponse, parser.RPAREN)
		if t.Body != nil {
			p.print(blank)
			p.fieldList(t.Body)
		} else {
			p.print(t.SemiPos, parser.SEMICOLON)
		}
	case *parser.Oneof:
		p.print(t.Pos(), parser.ONEOF, blank, t.Name, blank)
		p.fieldList(t.Fields)
	case *parser.OneofField:
		p.expr(t.Type)
		p.print(sep, t.Name, sep, parser.ASSIGN, blank, t.Number)
		if t.Options != nil {
			p.print(blank)
			p.expr(t.Options)
		}
		p.print(parser.SEMICOLON)
	case *parser.Extensions:
		p.print(t.Pos(), parser.EXTENSIONS, blank)
		p.expr(t.Ranges)
		if t.Options != nil {
			p.print(blank)
			p.expr(t.Options)
		}
		p.print(t.SemiPos, parser.SEMICOLON)
	case *parser.Extend:
		p.print(t.Pos(), parser.EXTEND, blank)
		p.expr(t.MessageType)
		p.print(blank)
		p.fieldList(t.Fields)
	case *parser.KeyValueExpr:
		p.expr(t)
		if srcIsOneLine && !last {
			p.print(blank)
		}
	case *parser.FieldOption:
		p.expr(t.Name)
		p.print(blank, parser.ASSIGN, blank)
		p.expr(t.Value)
		if t.CommaPos > 0 {
			p.print(parser.COMMA)
		}
		if srcIsOneLine && !last {
			p.print(blank)
		}
	default:
		panic("not implemented")
	}
}

func (p *printer) declList(list []parser.Expr) {
	tok := parser.ILLEGAL
	for _, d := range list {
		prev := tok
		switch d.(type) {
		case *parser.Import:
			tok = parser.IMPORT
		case *parser.Message:
			tok = parser.MESSAGE
		case *parser.Enum:
			tok = parser.ENUM
		case *parser.Package:
			tok = parser.PACKAGE
		case *parser.Option:
			tok = parser.OPTION
		case *parser.Service:
			tok = parser.SERVICE
		}
		if len(p.output) > 0 {
			min := 1
			if prev != tok {
				min = 2
			}
			p.linebreak(p.lineFor(d.Pos()), min)
		}
		p.expr(d)
	}
}

func (p *printer) lineFor(pos parser.Pos) int {
	return p.fset.Line(pos)
}

func (p *printer) linebreak(line, min int) (nbreaks int) {
	n := nlimit(line - p.pos.Line)
	if n < min {
		n = min
	}
	if n > 0 {
		nbreaks += n
		for ; n > 0; n-- {
			p.print(newline)
		}
	}
	return
}

func (p *printer) file(file *parser.File) {
	if file.Syntax != nil {
		p.print(file.Syntax.Pos(), parser.SYNTAX, blank, parser.ASSIGN, blank)
		p.expr(file.Syntax.Name)
		p.print(parser.SEMICOLON)
	}
	p.declList(file.Decls)
	p.print(newline)
}

type trimmer struct {
	output io.Writer
	state  int
	space  []byte
}

const (
	inSpace = iota
	inEscape
	inText
)

var aNewline = []byte("\n")

func (p *trimmer) Write(data []byte) (n int, err error) {
	m := 0
	var b byte
	for n, b = range data {
		if b == '\v' {
			b = '\t' // convert to htab
		}
		switch p.state {
		case inSpace:
			switch b {
			case '\t', ' ':
				p.space = append(p.space, b)
			case '\n', '\f':
				p.resetSpace()
				_, err = p.output.Write(aNewline)
			case tabwriter.Escape:
				_, err = p.output.Write(p.space)
				p.state = inEscape
				m = n + 1 // +1: skip tabwriter.Escape
			default:
				_, err = p.output.Write(p.space)
				p.state = inText
				m = n
			}
		case inEscape:
			if b == tabwriter.Escape {
				_, err = p.output.Write(data[m:n])
				p.resetSpace()
			}
		case inText:
			switch b {
			case '\t', ' ':
				_, err = p.output.Write(data[m:n])
				p.resetSpace()
				p.space = append(p.space, b)
			case '\n', '\f':
				_, err = p.output.Write(data[m:n])
				p.resetSpace()
				if err == nil {
					_, err = p.output.Write(aNewline)
				}
			case tabwriter.Escape:
				_, err = p.output.Write(data[m:n])
				p.state = inEscape
				m = n + 1 // +1: skip tabwriter.Escape
			}
		default:
			panic("unreachable")
		}
	}
	n = len(data)

	switch p.state {
	case inEscape, inText:
		_, err = p.output.Write(data[m:n])
		p.resetSpace()
	}
	return
}

func (p *trimmer) resetSpace() {
	p.state = inSpace
	p.space = p.space[0:0]
}

type Mode uint

const (
	RawMode Mode = 1 << iota
	TabIndent
	UseSpaces
)

type Config struct {
	Mode     Mode
	Tabwidth int

	IndentBytes  []byte
	FieldAlign   bool
	CommentAlign bool
}

func (cfg *Config) Fprint(output io.Writer, fset *parser.FileSet, node parser.Node) (err error) {
	if len(cfg.IndentBytes) == 0 {
		cfg.IndentBytes = append(cfg.IndentBytes, '\t')
	}

	p := newPrinter(cfg, fset)
	p.printNode(node)
	p.flush(parser.Position{Offset: infinity, Line: infinity}, parser.EOF)

	output = &trimmer{output: output}
	if cfg.Mode&RawMode == 0 {
		minwidth := cfg.Tabwidth
		padchar := byte('\t')
		if cfg.Mode&UseSpaces != 0 {
			padchar = ' '
		}
		twmode := tabwriter.DiscardEmptyColumns
		if cfg.Mode&TabIndent != 0 {
			minwidth = 0
			twmode |= tabwriter.TabIndent
		}
		output = tabwriter.NewWriter(output, minwidth, cfg.Tabwidth, 1, padchar, twmode)
	}

	if _, err = output.Write(p.output); err != nil {
		return
	}
	if tw, _ := output.(*tabwriter.Writer); tw != nil {
		err = tw.Flush()
	}
	return
}
