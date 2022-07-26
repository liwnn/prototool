package main

import (
	"bufio"
	"flag"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"strings"

	"github.com/liwnn/prototool/clonego"
)

var (
	message = flag.String("m", "", "message names split by ','")
	write   = flag.String("w", "", "write result to (source) file instead of stdout")
)

func main() {
	flag.Parse()

	args := flag.Args()
	if len(args) != 1 {
		return
	}
	arg := args[0]
	switch info, err := os.Stat(arg); {
	case err != nil:
		report(err)
	case !info.IsDir():
		var out io.Writer
		out = os.Stdout
		if *write != "" {
			f, err := os.Create(*write)
			if err != nil {
				report(err)
				return
			}
			out = bufio.NewWriter(f)
		}
		if err := processFile(arg, out); err != nil {
			report(err)
		}
		if bf, ok := out.(*bufio.Writer); ok {
			if err := bf.Flush(); err != nil {
				report(err)
			}
		}
	default:
		report(fmt.Errorf("%s is not a file", arg))
	}
}

func report(err error) {
	_, _ = fmt.Fprintf(os.Stderr, "%s\n", err)
}

func processFile(filename string, out io.Writer) error {
	text, err := ioutil.ReadFile(filename)
	if err != nil {
		return fmt.Errorf("ReadFile[%v] failed[%v]", filename, err)
	}

	if *message != "" {
		messages := strings.Split(*message, ",")
		// 按名字生成
		result, err := clonego.Gen(text, messages)
		if err != nil {
			return err
		}
		_, err = out.Write(result.Src)
		return err
	}
	// 全部生成
	result, err := clonego.GenAll(text)
	if err != nil {
		panic(err)
	}
	_, err = out.Write(result.Src)
	return err
}
