package fortran

import (
	"fmt"
	goast "go/ast"
	"go/token"
	"strconv"
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
	decls = p.transpileToNode()
	if len(p.errs) > 0 {
		for _, e := range p.errs {
			err = fmt.Errorf("%v\n%v", err, e)
		}
		fmt.Println("Errors:\n ", err)
		err = nil
		// return
	}

	p.ast.Decls = append(p.ast.Decls, decls...)
	return
}

func (p *parser) transpileToNode() (decls []goast.Decl) {

	if p.ident < 0 || p.ident >= len(p.ns) {
		p.errs = append(p.errs,
			fmt.Errorf("Ident is outside nodes: %d/%d", p.ident, len(p.ns)))
		return
	}

	switch p.ns[p.ident].tok {
	case SUBROUTINE:
		var decl goast.Decl
		decl = p.transpileSubroutine()
		decls = append(decls, decl)

	default:
		// move to next NEW_LINE
		p.addError("Cannot parse line: " + p.getLine())
	}

	return
}

func (p *parser) getLine() (line string) {
	if p.ident < 0 || p.ident >= len(p.ns) {
		p.addError("Cannot get line, ident = " + strconv.Itoa(p.ident))
		return
	}
	for ; p.ident < len(p.ns); p.ident++ {
		if p.ns[p.ident].tok == NEW_LINE {
			break
		}
		line += fmt.Sprintf(" %s", p.ns[p.ident].lit)
	}
	return
}

func (p *parser) transpileSubroutine() (decl goast.Decl) {
	var fd goast.FuncDecl
	fd.Type = &goast.FuncType{
		Params: &goast.FieldList{},
	}

	p.expect(SUBROUTINE)

	p.ident++
	p.expect(token.IDENT)
	name := p.ns[p.ident].lit

	p.ident++
	var hasParens bool = p.ns[p.ident].tok == token.LPAREN
	if hasParens {
		p.expect(token.LPAREN)

		// Parameters
		p.ident++
		fd.Type.Params.List = p.parseParamDecl()

		p.ident++
		p.expect(token.RPAREN)

		p.ident++
		p.expect(NEW_LINE)
	}

	p.ident++
	fd.Name = goast.NewIdent(name)
	fd.Body = &goast.BlockStmt{
		Lbrace: 1,
		List:   p.transpileListStmt(),
	}

	p.ident++
	// p.expect(END)

	decl = &fd
	return
}

func (p *parser) addError(msg string) {
	p.errs = append(p.errs, fmt.Errorf("%s", msg))
}

func (p *parser) expect(t token.Token) {
	if t != p.ns[p.ident].tok {
		panic(fmt.Errorf("Expect %s, but we have {{%s,%s}}",
			view(t), view(p.ns[p.ident].tok), p.ns[p.ident].lit))
	}
}

func (p *parser) transpileListStmt() (stmts []goast.Stmt) {
	for p.ident < len(p.ns) {
		if p.ns[p.ident].tok == END {
			// TODO
			break
		}
		stmt := p.parseStmt()
		if stmt == nil {
			// p.addError("stmt is nil in line : " + p.getLine())
			// break
			continue
		}
		stmts = append(stmts, stmt...)
	}
	return
}

func (p *parser) parseInit() (stmts []goast.Stmt) {

	identType := "int"
	switch p.ns[p.ident].tok {
	case LOGICAL:
		identType = "bool"
	case CHARACTER:
		identType = "byte"
	case COMPLEX:
		identType = "complex128"
	}

	p.ident++
	for ; p.ns[p.ident].tok != NEW_LINE; p.ident++ {
		switch p.ns[p.ident].tok {
		case token.IDENT:
			name := p.ns[p.ident].lit
			stmts = append(stmts, &goast.DeclStmt{
				Decl: &goast.GenDecl{
					Tok: token.VAR,
					Specs: []goast.Spec{
						&goast.ValueSpec{
							Names: []*goast.Ident{goast.NewIdent(name)},
							Type:  goast.NewIdent(identType),
						},
					},
				},
			})
		case token.MUL, token.INT:
			// TODO
		case token.COMMA:
			// ignore
		default:
			p.addError("Cannot parse INTEGER value : " + p.ns[p.ident].lit)
		}
	}

	return
}

func (p *parser) parseStmt() (stmts []goast.Stmt) {
	switch p.ns[p.ident].tok {
	case INTEGER, CHARACTER, COMPLEX, LOGICAL:

		stmts = append(stmts, p.parseInit()...)

	case token.RETURN:
		stmts = append(stmts, &goast.ReturnStmt{})
		p.ident++

		p.expect(NEW_LINE)
		p.ident++

	case END:
		// ignore
		p.ident++

		// TODO : p.expect(NEW_LINE)
		p.ident++

	default:
		fmt.Println("stmt:", p.getLine())
		p.ident++
	}
	return
}

func (p *parser) parseParamDecl() (fields []*goast.Field) {
	for ; p.ns[p.ident].tok != token.EOF; p.ident++ {
		var exit bool
		switch p.ns[p.ident].tok {
		case token.COMMA:
			// ignore
		case token.IDENT:
			id := p.ns[p.ident].lit
			field := &goast.Field{
				Names: []*goast.Ident{goast.NewIdent(id)},
				Type:  goast.NewIdent("int"),
			}
			fields = append(fields, field)
		case token.RPAREN:
			p.ident--
			exit = true
		default:
			p.addError("Cannot parse parameter decl " + p.ns[p.ident].lit)
			return
		}
		if exit {
			break
		}
	}
	return
}
