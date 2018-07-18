package fortran

import (
	"fmt"
	goast "go/ast"
	"go/token"
)

type node struct {
	tok token.Token
	lit string
}

type parser struct {
	sc    *Scanner
	ident int
	ns    []node
}

func (p *parser) prepare() (err error) {
	var last token.Token
	for {
		tok, lit := p.sc.Scan()
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

		p.ns = append(p.ns, node{
			tok: tok,
			lit: lit,
		})
		last = tok
	}

	if len(p.ns) > 0 && p.ns[0].tok == NEW_LINE {
		p.ns = p.ns[1:]
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
