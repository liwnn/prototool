package format

import (
	"bytes"

	"github.com/liwnn/prototool/parser"
	"github.com/liwnn/prototool/printer"
)

func Format(src []byte) ([]byte, error) {
	file := parser.NewFileInfo()
	parser := parser.NewParser(file, src)
	f := parser.ParseFile()

	cfg := printer.Config{
		Mode:         printer.UseSpaces | printer.TabIndent,
		Tabwidth:     8,
		IndentBytes:  []byte("  "),
		FieldAlign:   true,
		CommentAlign: true,
	}
	var buf bytes.Buffer
	if err := cfg.Fprint(&buf, file, f); err != nil {
		return nil, err
	}
	return buf.Bytes(), nil
}
