package fortran

import (
	"bytes"
	"fmt"
	"go/token"
	"strconv"
)

func (g goType) isArray() bool {
	// 	if g.baseType == "string" {
	// 		return len(g.arrayNode)-1 > 0
	// 	}
	return len(g.arrayNode) > 0
}

type goType struct {
	baseType  string
	arrayNode [][]node
}

func (g goType) getMinLimit(col int) (size int, ok bool) {
	if !g.isArray() {
		return
	}
	if col > len(g.arrayNode) {
		return
	}

	// Examples:
	// [[INT, `2`, {582 27}]]
	// [[-, `-`, {605 21}] [INT, `2`, {605 22}] [:, `:`, {605 23}] [INT, `3`, {605 24}]]
	index := -1
	for i := range g.arrayNode[col] {
		if g.arrayNode[col][i].tok == token.COLON { // :
			index = i
		}
	}
	if index < 0 {
		return 1, true
	}
	var value string
	for i := range g.arrayNode[col] {
		if i == index {
			break
		}
		value += string(g.arrayNode[col][i].b)
	}
	number, err := strconv.Atoi(value)
	if err != nil {
		return 1, true // TODO : need more clear error
	}
	return number, true
}

func (g goType) String() (s string) {
	s = g.getBaseType()
	for i := 0; i < len(g.arrayNode); i++ {
		// 		if g.baseType == "string" && i == 0 {
		// 			continue
		// 		}
		size, err := strconv.Atoi(nodesToString(g.arrayNode[i]))
		if err != nil {
			s = fmt.Sprintf("[]%s", s)
		} else {
			s = fmt.Sprintf("[%d]%s", size, s)
		}
	}
	return
}

func (g goType) getBaseType() string {
	// 	if g.baseType == "string" {
	// 		return "[]byte"
	// 	}
	return g.baseType
}

// From :
// CHARACTER(1) (32)
// To :
// CHARACTER *1 (32)
func fixType(nodes *[]node) {
	var counter int
	var end int
	for end = 1; end < len(*nodes); end++ {
		if (*nodes)[end].tok == ftNewLine {
			return
		}
		if (*nodes)[end].tok == token.LPAREN {
			break
		}
	}
	if end >= len(*nodes) {
		// for: "INTEGER"
		return
	}
	start := end
	for ; end < len(*nodes); end++ {
		if (*nodes)[end].tok == token.LPAREN {
			counter++
			continue
		}
		if (*nodes)[end].tok == token.RPAREN {
			counter--
		}
		if counter == 0 {
			break
		}
	}
	if len(*nodes)-1 < end {
		var buf bytes.Buffer
		for _, v := range *nodes {
			fmt.Fprintf(&buf, "%s\n", string(v.b))
		}
		err := fmt.Errorf("Not valid nodes size: %#v\n%s", *nodes, buf.String())
		panic(err)
	}
	if (*nodes)[end].tok != token.RPAREN {
		panic("Not acceptable end : " + string((*nodes)[end].b))
	}

	if len(*nodes)-end-1 <= 0 {
		return
	}
	if (*nodes)[end+1].tok != token.LPAREN {
		return
	}
	(*nodes)[start].tok, (*nodes)[start].b = token.MUL, []byte("*")
	(*nodes) = append((*nodes)[:end], (*nodes)[end+1:]...)
}

func parseType(nodes []node) (typ goType) {

	fixType(&nodes)

	typ.baseType = "undefined type"

	if len(nodes) == 0 {
		return
	}

	switch nodes[0].tok {
	case ftCharacter:
		// CHARACTER
		typ.baseType = "byte"
		nodes = nodes[1:]

		// CHARACTER * n,
		// n > 0
		if len(nodes) > 1 &&
			nodes[0].tok == token.MUL && nodes[1].tok == token.INT {
			if string(nodes[1].b) != "1" {
				typ.baseType = "byte" //"string"
				typ.arrayNode = append(typ.arrayNode, []node{nodes[1]})
			}
			nodes = nodes[2:]
		} else if len(nodes) > 0 && nodes[0].tok == token.MUL {
			nodes = nodes[1:]
		}
	case ftComplex:
		// COMPLEX or COMPLEX * 8
		typ.baseType = "complex128" // TODO: for minimaze type convection "complex64"
		nodes = nodes[1:]
		if len(nodes) > 1 &&
			nodes[0].tok == token.MUL &&
			nodes[1].tok == token.INT {
			switch string(nodes[1].b) {
			case "8": // COMPLEX * 8
				typ.baseType = "complex128" // TODO: for minimaze type convection "complex64"
			case "16": // COMPLEX * 16
				typ.baseType = "complex128"
			default:
				// COMPLEX * 32
				panic(fmt.Errorf(
					"Not support COMPLEX type : %s", string(nodes[1].b)))
			}
			nodes = nodes[2:]
		}

	case ftDouble:
		switch nodes[1].tok {
		case ftComplex: // COMPLEX * 16 or DOUBLE COMPLEX
			typ.baseType = "complex128"
		case ftPrecision: // REAL    * 16 or DOUBLE PRECISION
			typ.baseType = "float64"
		// TODO : panic("Not support DOUBLE PRECISION")
		default:
			panic(fmt.Errorf(
				"Not support DOUBLE type : %s", string(nodes[1].b)))
		}
		nodes = nodes[2:]

	case ftReal:
		// REAL or REAL * 4
		typ.baseType = "float64" // TODO : correct type "float32"
		nodes = nodes[1:]
		if len(nodes) > 1 &&
			nodes[0].tok == token.MUL &&
			nodes[1].tok == token.INT {
			switch string(nodes[1].b) {
			case "4": // REAL or REAL * 4
				typ.baseType = "float64" // TODO: for minimaze type convection "float32"
			case "8": // REAL * 8
				typ.baseType = "float64"
			default:
				// REAL * 16
				panic(fmt.Errorf(
					"Not support REAL type : %s", string(nodes[1].b)))
			}
			nodes = nodes[2:]
		}

	case ftInteger:
		// INTEGER or INTEGER * 4
		typ.baseType = "int"
		nodes = nodes[1:]
		if len(nodes) > 1 &&
			nodes[0].tok == token.MUL &&
			nodes[1].tok == token.INT {
			switch string(nodes[1].b) {
			case "2": // INTEGER * 2
				typ.baseType = "int16"
			case "4": // INTEGER * 4
				typ.baseType = "int32"
			case "8": // INTEGER * 8
				typ.baseType = "int64"
			default:
				panic(fmt.Errorf(
					"Not support INTEGER type : %s", string(nodes[1].b)))
			}
			nodes = nodes[2:]
		}

	case ftLogical:
		// LOGICAL or LOGICAL * 1
		typ.baseType = "bool"
		nodes = nodes[1:]
		if len(nodes) > 1 &&
			nodes[0].tok == token.MUL &&
			nodes[1].tok == token.INT {
			// LOGICAL * 1
			// LOGICAL * 2
			// LOGICAL * 4
			// LOGICAL * 8
			panic(fmt.Errorf(
				"Not support LOGICAL type : %s", string(nodes[1].b)))
			// nodes = nodes[2:]
		}
	}

	if len(nodes) == 0 {
		return
	}

	if nodes[0].tok != token.LPAREN {
		return
	}

	// Array:
	//  INTEGER (3)
	//  INTEGER (N+1,*)
	//  INTEGER (N+(1),*)
	//  LOGICAL ( - 1 : 1 )
	//  LOGICAL ( 0 : 1 , 0 : 1 )

	args, end := separateArgsParen(nodes)

	for _, a := range args {
		typ.arrayNode = append(typ.arrayNode, a)
	}

	nodes = nodes[end:]
	return
}
