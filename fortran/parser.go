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
	ast   goast.File
	ident int
	ns    []node

	errs []error
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

func (p *parser) parse() (err error) {
	err = p.prepare()
	if err != nil {
		return
	}

	p.ast.Name = goast.NewIdent("main")

	var decls []goast.Decl
	p.ident = 0
	decls, err = p.transpileToNode()
	if err != nil {
		return
	}

	p.ast.Decls = append(p.ast.Decls, decls...)
	return
}

func (p *parser) transpileToNode() (decls []goast.Decl, err error) {

	if p.ident < 0 || p.ident >= len(p.ns) {
		err = fmt.Errorf("Ident is outside nodes: %d/%d", p.ident, len(p.ns))
		return
	}

	switch p.ns[p.ident].tok {
	case SUBROUTINE:
		var decl goast.Decl
		decl, err = p.transpileSubroutine()
		if err != nil {
			return
		}
		decls = append(decls, decl)

	default:
		// move to next NEW_LINE
		var line string
		for ; p.ident < len(p.ns); p.ident++ {
			if p.ns[p.ident].tok == NEW_LINE {
				break
			}
			line += fmt.Sprintf(" %s", p.ns[p.ident].lit)
		}
		err = fmt.Errorf("Cannot parse line: `%s`", line)
	}

	return
}

func (p *parser) transpileSubroutine() (decl goast.Decl, err error) {
	p.expect(SUBROUTINE)
	p.ident++
	p.expect(token.IDENT)
	name := p.ns[p.ident].lit

	var fd goast.FuncDecl
	fd.Name = goast.NewIdent(name)
	fd.Type = &goast.FuncType{}
	fd.Body = &goast.BlockStmt{}

	decl = &fd
	return
}

func (p *parser) expect(t token.Token) {
	if t != p.ns[p.ident].tok {
		p.errs = append(p.errs, fmt.Errorf("Expect %s, but we have %s",
			view(t), view(p.ns[p.ident].tok)))
	}
}
