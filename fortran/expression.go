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

	var haveDoubleStar bool
	for _, n := range nodes {
		switch n.tok {
		case DOUBLE_STAR: // **
			haveDoubleStar = true
		}
	}
	if haveDoubleStar {
		p.addError("have double star" + ExprString(nodes))
	}

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

func (p *parser) isVariable(name string) bool {
	for _, v := range p.initVars {
		if v.name == name {
			return true
		}
	}
	return false
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

// TODO : add test for avoid comments /*...*/ in Go code
// TODO : add test for calc amount of TODO

// Examples:
//   SD2 / GAM ** 2
//   DSQRT ( ( DA / SCALE ) ** 2 + ( DB / SCALE ) ** 2 )
func (p *parser) fixDoubleStar(nodes *[]node) {
	var haveDoubleStar bool
	var pos int // saving last position of DOUBLE_STAR
	for i, n := range *nodes {
		switch n.tok {
		case DOUBLE_STAR: // **
			haveDoubleStar = true
			pos = i
		}
	}

	if !haveDoubleStar {
		return
	}

	// add package in source
	p.addImport("math")

	// Example of possible variables:
	// IDENT
	// ARRAY[...][...]
	// ( EXPRESSION )
	// FUNCTION (...)

	// separate expression on 2 parts
	// leftPart ** rightPart
	leftPart := (*nodes)[:pos]
	rightPart := (*nodes)[pos+1:]

	// separate left part on
	// leftOther leftVariable
	//          |
	//          +- leftSeparator
	var leftSeparator int
	for leftSeparator = len(leftPart) - 1; leftSeparator >= 0; leftSeparator-- {
		var br bool
		switch leftPart[leftSeparator].tok {
		case token.INT, token.FLOAT: // find Numbers
			br = true
		case token.IDENT: // find array name or function name
			br = true
		case token.RBRACK: // find ]
			// go to token [
			for {
				if leftPart[leftSeparator].tok == token.LBRACK {
					break
				}
				leftSeparator--
			}
		case token.RPAREN: // find ), so we have function or not
			// go to token (
			// inside parens can be another parens
			counter := 0
			for {
				if leftPart[leftSeparator].tok == token.RPAREN {
					counter++
				}
				if leftPart[leftSeparator].tok == token.LPAREN {
					counter--
				}
				if counter == 0 {
					break
				}
				leftSeparator--
			}
		default:
			p.addError("Cannot identify token in left part separation :" +
				view(leftPart[leftSeparator].tok))
			br = true
		}
		if br {
			break
		}
	}

	// separate right part on
	// rightVariable rightOther
	//              |
	//              +- rightSeparator
	var rightSeparator int
	for rightSeparator = 0; rightSeparator < len(rightPart); rightSeparator++ {
		var br bool
		switch rightPart[rightSeparator].tok {
		case token.INT, token.FLOAT:
			br = true
		case token.IDENT: // find IDENT, so it can be func or not
			if !p.isVariable(rightPart[rightSeparator].lit) {
				// function
				counter := 0
				for {
					if rightPart[rightSeparator].tok == token.LPAREN {
						counter++
					}
					if rightPart[rightSeparator].tok == token.RPAREN {
						counter--
					}
					if counter == 0 {
						break
					}
					rightSeparator++
				}
			}
			br = true
		case token.LPAREN: // find (
			counter := 0
			for {
				if rightPart[rightSeparator].tok == token.LPAREN {
					counter++
				}
				if rightPart[rightSeparator].tok == token.RPAREN {
					counter--
				}
				if counter == 0 {
					break
				}
				rightSeparator++
			}
			br = true
		default:
			p.addError("Cannot identify token in right part separation :" +
				view(rightPart[rightSeparator].tok))
			br = true
		}
		if br {
			break
		}
	}

	// combine expression by next formula:
	// leftOther math.Pow(leftVariable , rightVariable) rightOther
	var comb []node
	comb = append(comb, leftPart[:leftSeparator]...)
	comb = append(comb, []node{
		node{tok: token.IDENT, lit: "math.Pow"},
		node{tok: token.LPAREN, lit: "("},
	}...)
	comb = append(comb, leftPart[leftSeparator:]...)
	comb = append(comb, node{tok: token.COMMA, lit: ","})
	comb = append(comb, rightPart[:rightSeparator]...)
	comb = append(comb, node{tok: token.RPAREN, lit: ")"})
	comb = append(comb, rightPart[rightSeparator:]...)

	*nodes = comb

	// again checking, because we can have a few DOUBLE_STAR
	p.fixDoubleStar(nodes)
}
