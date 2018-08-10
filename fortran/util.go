package fortran

import (
	"go/token"
	"strings"
)

// Examples :
// ( 1, 2)
// ( *, N*2)
// ( 123, func(2,3))
func separateArgsParen(nodes []node) (args [][]node, end int) {
	if nodes[0].tok != token.LPAREN {
		panic("First symbol is not '(' : " + nodesToString(nodes))
	}
	var counter int
	args = append(args, []node{})
	for end = 0; end < len(nodes); end++ {
		if nodes[end].tok == token.LPAREN {
			counter++
			continue
		}
		if nodes[end].tok == token.COMMA && counter == 1 {
			args = append(args, []node{})
			continue
		}
		if nodes[end].tok == token.RPAREN {
			counter--
			if counter == 0 {
				break
			}
			continue
		}
		args[len(args)-1] = append(args[len(args)-1], nodes[end])
	}
	if nodes[end].tok != token.RPAREN {
		panic("Last symbol is not ')' : " + nodesToString(nodes))
	}
	end++
	return
}

func (p *parser) split(nodes *[]node, pos int) (
	leftOther, leftVariable []node,
	rightVariable, rightOther []node) {

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
			var isExternalFunction bool
			for _, f := range p.functionExternalName {
				if strings.ToUpper(f) == strings.ToUpper(string(leftPart[leftSeparator].b)) {
					isExternalFunction = true
					br = true
				}
			}
			if isExternalFunction {
				break
			}
			p.addError("Cannot identify token in left part separation :" +
				view(leftPart[leftSeparator].tok) + " in " + nodesToString(*nodes))
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
			// byte (...)
			isByte := false
			if v, ok := p.initVars[string(rightPart[rightSeparator].b)]; ok {
				if v.baseType == "byte" && len(v.arrayType) == 0 {
					if rightSeparator+1 < len(rightPart) &&
						rightPart[rightSeparator+1].tok == token.LPAREN {
						rightSeparator++
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
						isByte = true
						br = true
					}
				}
			}
			if isByte {
				break
			}

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
			} else {
				isArray := false
				if v, ok := p.initVars[string(rightPart[rightSeparator].b)]; ok {
					isArray = v.isArray()
				}
				if isArray {
					// it is array
					counter := 0
					rightSeparator++
					if rightSeparator+1 <= len(rightPart) {
						for {
							if rightPart[rightSeparator].tok == token.LBRACK {
								counter++
							}
							if rightPart[rightSeparator].tok == token.RBRACK {
								counter--
							}
							if counter == 0 {
								break
							}
							rightSeparator++
						}
					}
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

	rightSeparator++
	if rightSeparator >= len(rightPart) {
		return leftPart[:leftSeparator], leftPart[leftSeparator:],
			rightPart[:], []node{}
	}

	return leftPart[:leftSeparator], leftPart[leftSeparator:],
		rightPart[:rightSeparator], rightPart[rightSeparator:]
}
