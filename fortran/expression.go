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
		if p.ns[i].tok == ftNewLine {
			p.addError("NEW_LINE is not acceptable inside expression : " +
				nodesToString(p.ns[start:end]))
		}
	}

	in := p.ns[start:end]

	base := make([]node, len(in))
	copy(base, in)

	nodes := make([]node, len(in))
	copy(nodes, in)

	p.fixFakeParen(&nodes)
	p.fixArrayVariables(&nodes)
	p.fixDoubleStar(&nodes)
	p.fixString(&nodes)
	p.fixComplexValue(&nodes)
	p.fixConcatString(&nodes)

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
	if _, ok := p.initVars[name]; ok {
		return true
	}
	return false
}

func (p *parser) isArrayVariable(name string) bool {
	for n, goT := range p.initVars {
		if n == name && (goT.isArray() || goT.baseType == "string") {
			return true
		}
	}
	return false
}

// change  `(/` and `/)` to `((` and `))`
func (p *parser) fixFakeParen(nodes *[]node) {
	for i := 1; i < len(*nodes); i++ {
		if (*nodes)[i-1].tok == token.LPAREN && (*nodes)[i].tok == token.QUO {
			(*nodes)[i].tok, (*nodes)[i].b = (*nodes)[i-1].tok, (*nodes)[i-1].b
			continue
		}
		if (*nodes)[i-1].tok == token.QUO && (*nodes)[i].tok == token.RPAREN {
			(*nodes)[i-1].tok, (*nodes)[i-1].b = (*nodes)[i].tok, (*nodes)[i].b
			continue
		}
	}
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
	pos := 0
	for {
		for ; pos < len(*nodes); pos++ {
			if (*nodes)[pos].tok == token.IDENT &&
				p.isArrayVariable(string((*nodes)[pos].b)) {
				break
			}
		}
		if pos >= len(*nodes) {
			break
		}
		if pos+1 >= len(*nodes) || (*nodes)[pos+1].tok != token.LPAREN {
			// Example:
			//  ingeter c(10)
			//  call func(c) ! in function no LPAREN
			pos += 1
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
					(*nodes) = append((*nodes)[:i], append([]node{
						{
							tok: token.SUB,
							b:   []byte("-"),
						},
						{
							tok: token.INT,
							b:   []byte("1"),
						},
						{
							tok: token.RBRACK,
							b:   []byte("]"),
						},
					}, (*nodes)[i+1:]...)...)
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
						tok: token.SUB,
						b:   []byte("-"),
					},
					{
						tok: token.INT,
						b:   []byte("1"),
					},
					{
						tok: token.RBRACK,
						b:   []byte("]"),
					},
					{
						tok: token.LBRACK,
						b:   []byte("["),
					},
				}, (*nodes)[i+1:]...)...)
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
		case ftDoubleStar: // **
			haveDoubleStar = true
			pos = i
		}
	}

	if !haveDoubleStar {
		return
	}

	// add package in source
	p.addImport("math")

	leftOther, leftVariable, rightVariable, rightOther := p.split(nodes, pos)

	// combine expression by next formula:
	// leftOther math.Pow(leftVariable , rightVariable) rightOther
	var comb []node
	comb = append(comb, leftOther...)
	comb = append(comb, []node{
		{tok: token.IDENT, b: []byte("math.Pow")},
		{tok: token.LPAREN, b: []byte("(")},
	}...)
	comb = append(comb, leftVariable...)
	comb = append(comb, node{tok: token.COMMA, b: []byte(",")})
	comb = append(comb, rightVariable...)
	comb = append(comb, node{tok: token.RPAREN, b: []byte(")")})
	comb = append(comb, rightOther...)

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
				case ftNewLine, token.EOF:
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

func (p *parser) fixConcatString(nodes *[]node) {
	for {
		var pos int
		var found bool
		for i, n := range *nodes {
			if n.tok == ftStringConcat {
				pos = i
				found = true
			}
		}

		if !found {
			return
		}

		leftOther, leftVariable, rightVariable, rightOther := p.split(nodes, pos)

		// combine expression by next formula:
		// leftOther []byte{leftVariable , rightVariable} rightOther
		var comb []node
		comb = append(comb, leftOther...)
		comb = append(comb, []node{
			{tok: token.IDENT, b: []byte("append")},
			{tok: token.LPAREN, b: []byte("(")},
		}...)
		if leftVariable[0].tok == token.IDENT {
			if v, ok := p.initVars[string(leftVariable[0].b)]; ok {
				if v.baseType == "byte" && len(v.arrayType) == 0 {
					leftVariable = leftVariable[:1]
				}
			}
		}
		comb = append(comb, leftVariable...)
		comb = append(comb, node{tok: token.COMMA, b: []byte(",")})
		if rightVariable[0].tok == token.IDENT {
			if v, ok := p.initVars[string(rightVariable[0].b)]; ok {
				if v.baseType == "byte" && len(v.arrayType) == 0 {
					rightVariable = rightVariable[:1]
				}
			}
		}
		comb = append(comb, rightVariable...)
		comb = append(comb, node{tok: token.IDENT, b: []byte("...")})
		comb = append(comb, node{tok: token.RPAREN, b: []byte(")")})
		comb = append(comb, rightOther...)

		*nodes = comb
	}

}
