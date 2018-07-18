package fortran

import (
	"fmt"
	goast "go/ast"
	"go/token"
	"os"
)

type node struct {
	tok token.Token
	lit string
}

func prepare(filename string) (ns []node, err error) {
	file, err := os.Open(filename)
	if err != nil {
		return
	}
	defer file.Close()

	sc := NewScanner(file)

	var last token.Token
	for {
		tok, lit := sc.Scan()
		if tok == token.EOF {
			break
		}

		switch tok {
		case token.COMMENT:
			continue
		}

		if last == NEW_LINE && tok == NEW_LINE {
			continue
		}

		ns = append(ns, node{
			tok: tok,
			lit: lit,
		})
		last = tok
	}

	return
}

func a(ns []node) (out string) {
	for _, n := range ns {
		switch n.tok {
		case NEW_LINE:
			out += fmt.Sprintf("\n")
		default:
			out += fmt.Sprintf(" %v", n.lit)
		}
	}
	return
}

func p(ns []node) (ast goast.File, err error) {
	ast.Name = goast.NewIdent("main")
	return
}
