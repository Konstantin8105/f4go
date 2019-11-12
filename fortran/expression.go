package fortran

import (
	"fmt"
	goast "go/ast"
	goparser "go/parser"
	"go/token"
	"strconv"
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
	return strings.TrimSpace(str)
}

func (p *parser) parseExpr(start, end int) (expr goast.Expr) {
	return p.parseExprNodes(p.ns[start:end])
}

func (p *parser) parseExprNodes(in []node) (expr goast.Expr) {

	for i := 0; i < len(in); i++ {
		if in[i].tok == ftNewLine {
			p.addError("NEW_LINE is not acceptable inside expression : " +
				nodesToString(in))
		}
	}

	base := make([]node, len(in))
	copy(base, in)

	nodes := make([]node, len(in))
	copy(nodes, in)

	p.fixFakeParen(&nodes)
	p.fixArrayVariables(&nodes)
	if m, ok := p.fixVectorExplode(&nodes); ok {
		nodes = m
	}
	p.fixDoubleStar(&nodes)
	p.fixString(&nodes)
	p.fixComplexValue(&nodes)
	p.fixIdent(&nodes)
	p.fixConcatString(&nodes)

	str := nodesToString(nodes)

	//use std package go/parser for change to parse expression
	ast, err := goparser.ParseExpr(str)
	if err != nil {
		out := "Cannot parse Expression : "
		if len(base) > 0 {
			out += fmt.Sprintf("pos {%v} ", base[0].pos)
		}
		out += fmt.Sprintf("`%s`\t`%s`\t`%s`",
			nodesToString(base), str, err)
		return goast.NewIdent(str)
	}

	p.fixComplexRealOperation(ast)

	return ast
}

func (p *parser) isVariable(name string) bool {
	_, ok := p.initVars.get(name)
	return ok
}

func (p *parser) isArrayVariable(name string) bool {
	if v, ok := p.initVars.get(name); ok {
		if v.typ.isArray() || v.typ.baseType == "string" {
			return true
		}
	}
	return false
}

// change  `(/` and `/)` to `((` and `))`
func (p *parser) fixFakeParen(nodes *[]node) {
	var foundBegin bool
	var begin int
	for i := 1; i < len(*nodes); i++ {
		if !foundBegin {
			if (*nodes)[i-1].tok == token.LPAREN && (*nodes)[i].tok == token.QUO {
				begin = i
				foundBegin = !foundBegin
			}
			continue
		}
		if (*nodes)[i-1].tok == token.QUO && (*nodes)[i].tok == token.RPAREN {
			foundBegin = false
			(*nodes)[begin].tok, (*nodes)[begin].b = token.LPAREN, []byte("(")
			(*nodes)[i-1].tok, (*nodes)[i-1].b = token.RPAREN, []byte(")")
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
		var v varInitialization
		for ; pos < len(*nodes); pos++ {
			if (*nodes)[pos].tok == token.IDENT {
				var ok bool
				if v, ok = p.initVars.get(string((*nodes)[pos].b)); ok &&
					(p.getArrayLen(v.name) > 0 || v.typ.baseType == "string") {
					break
				}
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
		pos += 1
		args, end := separateArgsParen((*nodes)[pos:])

		// inject nodes
		var inject []node
		for i, a := range args {
			begin := p.getArrayBegin(v.name, i)
			for j := range a {
				if a[j].tok == token.COLON {
					// from : S[ I:I ]
					// to   : S[ I ]
					if j == 1 && len(a) == 3 {
						a[j].tok, a[j].b = token.STRING, []byte(" ")
						a[j+1].tok, a[j+1].b = token.STRING, []byte(" ")
						// a = append(a[:j], append([]node{
						// 	{tok: token.SUB, b: []byte("-")},
						// 	{tok: token.LPAREN, b: []byte("(")},
						// 	{
						// 		tok: token.INT,
						// 		b:   []byte(strconv.Itoa(begin)),
						// 	},
						// 	{tok: token.RPAREN, b: []byte(")")},
						// }, a[j:]...)...)
					}
				}
			}
			inject = append(inject, node{tok: token.LBRACK, b: []byte("[")})
			inject = append(inject, a...)
			inject = append(inject, []node{
				{tok: token.SUB, b: []byte("-")},
				{tok: token.LPAREN, b: []byte("(")},
				{
					tok: token.INT,
					b:   []byte(strconv.Itoa(begin)),
				},
				{tok: token.RPAREN, b: []byte(")")},
			}...)
			inject = append(inject, node{tok: token.RBRACK, b: []byte("]")})
		}

		(*nodes) = append((*nodes)[:pos], append(inject, (*nodes)[pos+end:]...)...)
		pos += end
	}
}

// Example:
// ( ( D ( I , J ) , J = 1 , 4 ) , I = 1 , 4 )
// =                             = = = = = = =
//
// Sign is change behavior ( KFIN ( 1 , J ) , J = - 40 , 40 )
func (p *parser) fixVectorExplode(nodes *[]node) (merge []node, ok bool) {
	ns, ok := explodeFor(*nodes)
	if !ok {
		return
	}
	for i := range ns {
		merge = append(merge, ns[i]...)
		if i != len(ns)-1 {
			merge = append(merge, node{
				tok: token.COMMA,
				b:   []byte{','},
			})
		}
	}
	return merge, true
}

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
			if (*nodes)[i].b[0] == '\'' {
				(*nodes)[i].b[0] = '"'
			}
			if (*nodes)[i].b[len((*nodes)[i].b)-1] == '\'' {
				(*nodes)[i].b[len((*nodes)[i].b)-1] = '"'
			}
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
		// leftOther append(append([]byte,leftVariable),rightVariable) rightOther
		//           ---------------------            --             -
		var comb []node
		comb = append(comb, leftOther...)
		comb = append(comb, []node{
			{tok: token.IDENT, b: []byte("append")},
			{tok: token.LPAREN, b: []byte("(")},
			{tok: token.IDENT, b: []byte("append")},
			{tok: token.LPAREN, b: []byte("(")},
			{tok: token.LBRACK, b: []byte("[")},
			{tok: token.RBRACK, b: []byte("]")},
			{tok: token.IDENT, b: []byte("byte")},
			{tok: token.LBRACE, b: []byte("{")},
			{tok: token.RBRACE, b: []byte("}")},
			{tok: token.COMMA, b: []byte(",")},
		}...)
		comb = append(comb, leftVariable...)
		comb = append(comb, node{tok: token.RPAREN, b: []byte(")")})
		comb = append(comb, node{tok: token.COMMA, b: []byte(",")})
		comb = append(comb, rightVariable...)
		comb = append(comb, node{tok: token.RPAREN, b: []byte(")")})
		comb = append(comb, rightOther...)

		*nodes = comb
	}
}

func (p *parser) fixIdent(nodes *[]node) {
	for i := len(*nodes) - 1; i >= 0; i-- {
		if (*nodes)[i].tok != token.IDENT {
			continue
		}
		if i+1 < len(*nodes) && (*nodes)[i+1].tok == token.LPAREN {
			// for function
			continue
		}
		if s := string((*nodes)[i].b); s == "false" || s == "true" {
			continue
		}

		// from | IDENT  |
		// to   | LPAREN | STAR | IDENT | RPAREN |
		var comb []node
		comb = append(comb, (*nodes)[:i]...)
		comb = append(comb, node{tok: token.LPAREN, b: []byte("(")})
		comb = append(comb, node{tok: token.MUL, b: []byte("*")})
		comb = append(comb, (*nodes)[i])
		comb = append(comb, node{tok: token.RPAREN, b: []byte(")")})
		comb = append(comb, (*nodes)[i+1:]...)
		*nodes = comb
		i += 1
	}
}

// Example:
//
//  0  *ast.BinaryExpr {
//  1  .  X: *ast.Ident {
//  3  .  .  Name: "R"
//  8  .  }
//  9  .  OpPos: -
// 10  .  Op: *
// 11  .  Y: *ast.Ident {
// 13  .  .  Name: "CR"
// 15  .  }
// 16  }
//
//  0  *ast.BinaryExpr {
//  1  .  X: *ast.ParenExpr {
//  3  .  .  X: *ast.StarExpr {
//  5  .  .  .  X: *ast.Ident {
//  7  .  .  .  .  Name: "R"
// 12  .  .  .  }
// 13  .  .  }
// 15  .  }
// 16  .  OpPos: -
// 17  .  Op: *
// 18  .  Y: *ast.ParenExpr {
// 20  .  .  X: *ast.StarExpr {
// 22  .  .  .  X: *ast.Ident {
// 24  .  .  .  .  Name: "CR"
// 26  .  .  .  }
// 27  .  .  }
// 29  .  }
// 30  }
//
func (p *parser) fixComplexRealOperation(ast goast.Expr) {
	if be, ok := ast.(*goast.BinaryExpr); ok {
		xIsComplex, xOk := p.isComplex(be.X)
		yIsComplex, yOk := p.isComplex(be.Y)
		if xOk && yOk {
			if xIsComplex && !yIsComplex {
				be.Y = &goast.CallExpr{
					Fun: goast.NewIdent("complex"),
					Args: []goast.Expr{
						be.Y,
						goast.NewIdent("0"),
					},
				}
			}
			if !xIsComplex && yIsComplex {
				be.X = &goast.CallExpr{
					Fun: goast.NewIdent("complex"),
					Args: []goast.Expr{
						be.X,
						goast.NewIdent("0"),
					},
				}
			}
		}
	}
}

// Example:
//
//  0  *ast.CallExpr {
//  3  .  .  Name: "DBLE"
//  8  .  }
// 10  .  Args: []ast.Expr (len = 1) {
// 11  .  .  0: *ast.Ident {
// 13  .  .  .  Name: "CR"
// 15  .  .  }
// 16  .  }
// 19  }
//
// 0  *ast.Ident {
// 2  .  Name: "A"
// 7  }
//
// 18 *ast.ParenExpr {
// 20 X: *ast.StarExpr {
// 22 .  X: *ast.Ident {
// 24 .  .  Name: "CR"
// 26 .  }
// 27 }
//
func (p *parser) isComplex(e goast.Expr) (isComplex, ok bool) {
	if par, ok := e.(*goast.ParenExpr); ok {
		if st, ok := par.X.(*goast.StarExpr); ok {
			if id, ok := st.X.(*goast.Ident); ok {
				if v, ok := p.initVars.get(id.Name); ok {
					if strings.Contains(v.typ.getBaseType(), "complex") {
						return true, true
					} else {
						return false, true
					}
				}
			}
		}
	}
	if id, ok := e.(*goast.Ident); ok {
		if v, ok := p.initVars.get(id.Name); ok {
			if strings.Contains(v.typ.getBaseType(), "complex") {
				return true, true
			} else {
				return false, true
			}
		}
	}
	if ce, ok := e.(*goast.CallExpr); ok {
		if id, ok := ce.Fun.(*goast.Ident); ok {
			switch id.Name {
			case "DBLE":
				return false, true
			}
		}
	}
	return false, false
}
