package parser

import "fmt"

type Visitor interface {
	Visit(node Node) (w Visitor)
}

func Walk(v Visitor, node Node) {
	if t := v.Visit(node); t == nil {
		return
	}

	switch n := node.(type) {
	case *File:
		Walk(v, n.Syntax)
		for _, spec := range n.Decls {
			Walk(v, spec)
		}
	case *Message:
		for _, field := range n.Fields.List {
			switch e := field.(type) {
			case *Message, *Enum:
				Walk(v, e)
			}
		}
	case *Enum:
	case *Service:
	case *Extend:
	case *Import:
	case *Package:
	case *Option:
	case *Syntax:
	default:
		panic(fmt.Sprintf("%T", n))
	}
}
