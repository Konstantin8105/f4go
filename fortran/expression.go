package fortran

import (
	"fmt"
	goast "go/ast"
	goparser "go/parser"
	"go/token"
	"strings"
)

func ExprString(nodes []node) (str string) {
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
	return str
}

func (p *parser) parseBinaryExpr(in []node) goast.Expr {

	base := make([]node, len(in))
	copy(base, in)

	nodes := make([]node, len(in))
	copy(nodes, in)

	p.fixArrayVariables(&nodes)
	p.fixDoubleStar(&nodes)

	str := ExprString(nodes)

	//use std package go/parser for change to parse expression
	ast, err := goparser.ParseExpr(str)
	if err != nil {
		p.addError("Cannot parse Expression : " + ExprString(base) +
			"\t" + str +
			"\t" + fmt.Sprintf("%v", err))
		return goast.NewIdent(str)
	}
	return ast
}

func (p *parser) isArrayVariable(name string) bool {
	for _, v := range p.initVars {
		if v.name == name && strings.Contains(v.typ, "[") {
			return true
		}
	}
	return false
}

// fixArrayVariables - change tokens of array
// From : ... | NAME | ( | I |   ,   | J | ) | ...
// To   : ... | NAME | [ | I | ] | [ | J | ] | ...
func (p *parser) fixArrayVariables(nodes *[]node) {
	var positions []int
	// find all arrays
	for i, n := range *nodes {
		if n.tok == token.IDENT && p.isArrayVariable(n.lit) {
			positions = append(positions, i)
		}
	}

	if len(positions) == 0 {
		return
	}

	// modify tokens
	var step int
	for _, pos := range positions {
		pos += step
		if (*nodes)[pos+1].tok != token.LPAREN {
			p.addError("Cannot found LPAREN in array" + fmt.Sprintf("%v", *nodes))
		}
		(*nodes)[pos+1].tok, (*nodes)[pos+1].lit = token.LBRACK, "["
		var counter int = 1
		var end int
		for i := pos + 1; i < len(*nodes); i++ {
			if (*nodes)[i].tok == token.LPAREN {
				counter++
				continue
			}
			if (*nodes)[i].tok == token.RPAREN {
				counter--
				if counter == 0 {
					(*nodes)[i].tok, (*nodes)[i].lit = token.RBRACK, "]"
					end = i
					break
				}
			}
		}

		// insert comma
		// if comma is exist, then between nodes pos+1 and end
		for i := pos + 1; i < end; i++ {
			if (*nodes)[i].tok == token.COMMA {
				*nodes = append((*nodes)[:i], append([]node{
					node{
						tok: token.RBRACK,
						lit: "]",
					},
					node{
						tok: token.LBRACK,
						lit: "[",
					},
				}, (*nodes)[i+1:]...)...)
				step++
			}
		}
	}
}

// Examples:
//   SD2 / GAM ** 2
//   DSQRT ( ( DA / SCALE ) ** 2 + ( DB / SCALE ) ** 2 )
func (p *parser) fixDoubleStar(nodes *[]node) {
	var haveDoubleStar bool
	for _, n := range *nodes {
		switch n.tok {
		case DOUBLE_STAR: // **
			haveDoubleStar = true
		}
	}

	if !haveDoubleStar {
		return
	}

	p.addError("have double star" + ExprString(*nodes))
}
