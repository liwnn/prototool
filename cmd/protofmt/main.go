package main

import (
	"bytes"
	"flag"
	"fmt"
	"io"
	"io/fs"
	"os"
	"path/filepath"
	"strings"

	"github.com/liwnn/prototool/parser"
	"github.com/liwnn/prototool/printer"
)

var (
	write = flag.Bool("w", false, "write result to (source) file instead of stdout")
	align = flag.Bool("a", false, "align code")
)

func main() {
	flag.Parse()

	args := flag.Args()
	if len(args) == 0 {
		process(".")
		return
	}
	for _, arg := range args {
		process(arg)
	}
}

func process(arg string) {
	switch info, err := os.Stat(arg); {
	case err != nil:
		report(err)
	case !info.IsDir():
		// Non-directory arguments are always formatted.
		if err := processFile(arg, os.Stdout); err != nil {
			report(err)
		}
	default:
		// Directories are walked, ignoring non-Go files.
		if err := filepath.WalkDir(arg, visitFile); err != nil {
			report(err)
		}
	}
}

func report(err error) {
	_, _ = fmt.Fprintf(os.Stderr, "%s\n", err)
}

func processFile(filename string, out io.Writer) error {
	src, err := os.ReadFile(filename)
	if err != nil {
		return err
	}
	if len(src) == 0 {
		return nil
	}

	defer func() {
		if e := recover(); e != nil {
			fmt.Printf("Failure: %v:%v", filename, e)
			os.Exit(1)
		}
	}()

	file := parser.NewFileInfo()
	p := parser.NewParser(file, src)
	f := p.ParseFile()

	cfg := printer.Config{
		Mode:        printer.UseSpaces | printer.TabIndent,
		Tabwidth:    8,
		IndentBytes: []byte("  "),
	}
	if *align {
		cfg.FieldAlign = true
		cfg.CommentAlign = true
	}

	var buf bytes.Buffer
	if err := cfg.Fprint(&buf, file, f); err != nil {
		return err
	}
	res := buf.Bytes()
	if !bytes.Equal(src, res) {
		if *write {
			return os.WriteFile(filename, res, 0644)
		}
		_, err = out.Write(res)
		return err
	}
	return nil
}

func visitFile(path string, f fs.DirEntry, err error) error {
	if err != nil || !isProtoFile(f) {
		return err
	}
	return processFile(path, os.Stdout)
}

func isProtoFile(f fs.DirEntry) bool {
	name := f.Name()
	return !f.IsDir() && !strings.HasPrefix(name, ".") && strings.HasSuffix(name, ".proto")
}
