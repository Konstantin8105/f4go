package fortran

import (
	goast "go/ast"
	goparser "go/parser"
	"go/token"
)

func (p *parser) parseBinaryExpr(nodes []node) goast.Expr {
	var str string

	for _, n := range nodes {
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

	var haveDoubleStar, haveParen bool
	for _, n := range nodes {
		switch n.tok {
		case DOUBLE_STAR: // **
			haveDoubleStar = true
		case token.LPAREN:
			haveParen = true
		}
	}

	//TODO add support of array
	//TODO change to parseExpr from go package
	//TODO check operation **

	if !haveDoubleStar && !haveParen {
		ast, err := goparser.ParseExpr(str)
		if err == nil {
			return ast
		}
	}
	p.addError("Cannot parse Expression : " + str)

	return goast.NewIdent(str)
}
