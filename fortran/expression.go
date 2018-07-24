package fortran

import (
	goast "go/ast"
	goparser "go/parser"
	"go/token"
)

func (p *parser) parseBinaryExpr(nodes []node) goast.Expr {
	var str string

	var haveDoubleStar bool

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
		case DOUBLE_STAR: // **
			str += " " + n.lit
			haveDoubleStar = true
		default:
			str += " " + n.lit
		}
	}

	//TODO add support of array
	//TODO change to parseExpr from go package
	//TODO check operation **

	if !haveDoubleStar {
		ast, err := goparser.ParseExpr(str)
		if err == nil {
			return ast
		}
	}
	p.addError("Cannot parse Expression : " + str)

	return goast.NewIdent(str)
}
