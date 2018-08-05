package fortran

import (
	"fmt"
	goast "go/ast"
	goparser "go/parser"
	"go/token"
	"strings"
)

func nodesToString(nodes []node) (str string) {
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
			str += " " + string(n.b)
		}
	}
	return str
}

func (p *parser) parseExpr(start, end int) (expr goast.Expr) {

	for i := start; i < end; i++ {
		if p.ns[i].tok == NEW_LINE {
			p.addError("NEW_LINE is not acceptable inside expression : " +
				nodesToString(p.ns[start:end]))
		}
	}

	in := p.ns[start:end]

	defer func() {
		if r := recover(); r != nil {
			p.addError(fmt.Sprintf("%v", r))
			expr = goast.NewIdent(nodesToString(in))
		}
	}()

	base := make([]node, len(in))
	copy(base, in)

	nodes := make([]node, len(in))
	copy(nodes, in)

	p.fixArrayVariables(&nodes)
	p.fixDoubleStar(&nodes)
	p.fixString(&nodes)
	p.fixComplexValue(&nodes)

	str := nodesToString(nodes)

	//use std package go/parser for change to parse expression
	ast, err := goparser.ParseExpr(str)
	if err != nil {
		p.addError("Cannot parse Expression : " +
			fmt.Sprintf("`%s`\t`%s`\t`%s`", nodesToString(base), str, err))
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
		if v.name == name && v.isArray() {
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
		if n.tok == token.IDENT && p.isArrayVariable(string(n.b)) {
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
		if pos+1 >= len(*nodes) || (*nodes)[pos+1].tok != token.LPAREN {
			// Example:
			//  ingeter c(10)
			//  call func(c) ! in function no LPAREN
			continue
		}
		(*nodes)[pos+1].tok, (*nodes)[pos+1].b = token.LBRACK, []byte("[")
		counter := 1
		var end int
		for i := pos + 1; i < len(*nodes); i++ {
			if (*nodes)[i].tok == token.LPAREN {
				counter++
				continue
			}
			if (*nodes)[i].tok == token.RPAREN {
				counter--
				if counter == 0 {
					(*nodes)[i].tok, (*nodes)[i].b = token.RBRACK, []byte("]")
					end = i
					break
				}
			}
		}

		// insert comma
		// if comma is exist, then between nodes pos+1 and end
		counter = 1
		for i := pos + 1; i < end; i++ {
			if (*nodes)[i].tok == token.LPAREN {
				counter++
			}
			if (*nodes)[i].tok == token.RPAREN {
				counter--
			}
			if counter == 1 && (*nodes)[i].tok == token.COMMA {
				*nodes = append((*nodes)[:i], append([]node{
					{
						tok: token.RBRACK,
						b:   []byte("]"),
					},
					{
						tok: token.LBRACK,
						b:   []byte("["),
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
	//  IDENT
	//  INT
	//  FLOAT
	//  ARRAY[...][...]
	//  ( EXPRESSION )
	//  FUNCTION (...)

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
		case token.ADD, // +
			token.SUB, // -
			token.MUL, // *
			token.QUO, // /
			token.REM, // %

			token.AND,     // &
			token.OR,      // |
			token.XOR,     // ^
			token.SHL,     // <<
			token.SHR,     // >>
			token.AND_NOT, // &^

			token.LAND, // &&
			token.LOR,  // ||

			token.EQL,    // ==
			token.LSS,    // <
			token.GTR,    // >
			token.ASSIGN, // =
			token.NOT,    // !

			token.NEQ, // !=
			token.LEQ, // <=
			token.GEQ, // >=

			token.LPAREN: // (

			leftSeparator++
			br = true

		default:
			p.addError("Cannot identify token in left part separation :" +
				view(leftPart[leftSeparator].tok))
			br = true
		}
		if br {
			break
		}
	}
	if leftSeparator < 0 {
		leftSeparator = 0
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
			if !p.isVariable(string(rightPart[rightSeparator].b)) {
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
		{tok: token.IDENT, b: []byte("math.Pow")},
		{tok: token.LPAREN, b: []byte("(")},
	}...)
	comb = append(comb, leftPart[leftSeparator:]...)
	comb = append(comb, node{tok: token.COMMA, b: []byte(",")})
	comb = append(comb, rightPart[:rightSeparator+1]...)
	comb = append(comb, node{tok: token.RPAREN, b: []byte(")")})
	comb = append(comb, rightPart[rightSeparator+1:]...)

	*nodes = comb

	// again checking, because we can have a few DOUBLE_STAR
	p.fixDoubleStar(nodes)
}

func (p *parser) fixString(nodes *[]node) {
	for i := range *nodes {
		if (*nodes)[i].tok == token.STRING {
			(*nodes)[i].b = []byte(strings.Replace(string((*nodes)[i].b), "'", "\"", -1))
		}
	}
}

// parse complex init
// From :
// ( 1.0E+0 , 0.0E+0 )
// To :
//  1.0E+0 + 0.0E+0i
//
// Common pattern is:
//  ( .leftNodes. , .rightNodes. )
// Example of .nodes. :
//  IDENT
//  INT
//  FLOAT
//  ARRAY[...][...]
//  ( EXPRESSION )
//  EXPRESSION
//  FUNCTION (...)
func (p *parser) fixComplexValue(nodes *[]node) {
	var start, comma, end int
	var find bool
	for i := range *nodes {
		if i != 0 {
			if (*nodes)[i-1].tok == token.IDENT {
				// this is function
				continue
			}
		}
		// find (
		start = i
		if (*nodes)[i].tok != token.LPAREN {
			continue
		}
		i++
		// create function
		getNodes := func() bool {
			var j int
			for j = i; j < len(*nodes); j++ {
				var exit bool
				switch (*nodes)[j].tok {
				case token.LPAREN:
					counter := 0
					for ; j < len(*nodes); j++ {
						switch (*nodes)[j].tok {
						case token.LPAREN:
							counter++
						case token.RPAREN:
							counter--
						}
						if counter == 0 {
							break
						}
					}
				case NEW_LINE, token.EOF:
					return false
				case token.COMMA, token.RPAREN:
					exit = true
				}
				if exit {
					break
				}
			}
			i = j - 1
			return true
		}
		// find leftNodes
		if !getNodes() {
			continue
		}
		i++
		// is comma?
		comma = i
		if (*nodes)[i].tok != token.COMMA {
			continue
		}
		i++
		// find rightNodes
		if !getNodes() {
			continue
		}
		i++
		// if )
		end = i
		if (*nodes)[i].tok != token.RPAREN {
			continue
		}
		// all part of complex value if found
		find = true
		break
	}
	if !find {
		return
	}
	// combine new complex value interpretation
	var comb []node
	comb = append(comb, (*nodes)[:start]...)

	comb = append(comb, node{tok: token.LPAREN, b: []byte("(")})
	comb = append(comb, (*nodes)[start:comma]...)
	comb = append(comb, node{tok: token.ADD, b: []byte("+")})
	comb = append(comb, node{tok: token.LPAREN, b: []byte("(")})
	comb = append(comb, (*nodes)[comma+1:end]...)
	comb = append(comb, node{tok: token.RPAREN, b: []byte(")")})
	comb = append(comb, node{tok: token.MUL, b: []byte("*")})
	comb = append(comb, node{tok: token.FLOAT, b: []byte("1i")})
	comb = append(comb, node{tok: token.RPAREN, b: []byte(")")})
	comb = append(comb, (*nodes)[end:]...)

	*nodes = comb

	p.fixComplexValue(nodes)
}
