package fortran

import (
	goast "go/ast"
)

type expression struct {
	nodes []node
	p     *parser
	ident int
}

func (p *parser) parseBinaryExpr(nodes []node) goast.Expr {
	e := expression{
		nodes: nodes,
		p:     p,
	}
	return e.parse()
}

func (e *expression) parse() goast.Expr {
	x := e.parseUnaryExpr(lhs)
	for {
		op, oprec := p.tokPrec()
		if oprec < prec1 {
			return x
		}
		// pos := p.expect(op)
		if lhs {
			// p.resolve(x)
			lhs = false
		}
		y := p.parseBinaryExpr(false, oprec+1)
		x = &goast.BinaryExpr{X: x, Op: op, Y: y}
	}
}

/*
		var str string
		for _, n := range pExpr {
			switch n.tok {
			case
				token.LSS,    // <
				token.GTR,    // >
				token.LEQ,    // <=
				token.GEQ,    // >=
				token.NOT,    // !
				token.NEQ,    // !=
				token.EQL,    // ==
				token.LAND,   // &&
				token.LOR,    // ||
				token.ASSIGN: // =
				str += " " + view(n.tok)
			default:
				str += " " + n.lit
			}
		}

		fmt.Println("Expr : ", str)
		//TODO add support of array
		//TODO change to parseExpr from go package
		//TODO check operation **
		return goast.NewIdent(str)
}

func (p *parser) parseUnaryExpr() goast.Expr {

	switch p.tok {
	case token.ADD, token.SUB, token.NOT, token.XOR, token.AND:
		pos, op := p.pos, p.tok
		p.next()
		x := p.parseUnaryExpr(false)
		return &goast.UnaryExpr{OpPos: pos, Op: op, X: p.checkExpr(x)}

	case token.MUL:
		// pointer type or unary "*" expression
		pos := p.pos
		p.next()
		x := p.parseUnaryExpr(false)
		return &goast.StarExpr{Star: pos, X: p.checkExprOrType(x)}
	}

	return p.parsePrimaryExpr(lhs)
}

const (
	LowestPrec  = 0 // non-operators
	UnaryPrec   = 6
	HighestPrec = 7
)
	// if p.trace {
	// 	defer un(trace(p, "BinaryExpr"))
	// }

	x := p.parseUnaryExpr(lhs)
	for {
		op, oprec := p.tokPrec()
		if oprec < prec1 {
			return x
		}
		// pos := p.expect(op)
		if lhs {
			// p.resolve(x)
			lhs = false
		}
		y := p.parseBinaryExpr(false, oprec+1)
		x = &goast.BinaryExpr{X: x, Op: op, Y: y}
	}
}

// If lhs is set and the result is an identifier, it is not resolved.

func (p *parser) tokPrec() (token.Token, int) {
	tok := p.tok
	if p.inRhs && tok == token.ASSIGN {
		tok = token.EQL
	}
	return tok, tok.Precedence()
}
*/
