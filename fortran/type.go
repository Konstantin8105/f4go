package fortran

import (
	"fmt"
	"go/token"
	"strconv"
)

type initialVar struct {
	name string
	typ  goType
}

func (in initialVar) isArray() bool {
	return len(in.typ.arrayType) > 0
}

type goType struct {
	baseType  string
	arrayType []int
}

func (g goType) String() (s string) {
	s = g.baseType
	for _, size := range g.arrayType {
		if size == -1 {
			s = fmt.Sprintf("[]%s", s)
		} else {
			s = fmt.Sprintf("[%d]%s", size, s)
		}
	}
	return
}

func parseType(nodes []node) (typ goType) {

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
			typ.baseType = "string"
			nodes = nodes[2:]
		}
	case ftComplex:
		// COMPLEX or COMPLEX * 8
		typ.baseType = "complex64"
		nodes = nodes[1:]
		if len(nodes) > 1 &&
			nodes[0].tok == token.MUL &&
			nodes[1].tok == token.INT {
			switch string(nodes[1].b) {
			case "8": // COMPLEX * 8
				typ.baseType = "complex64"
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
		nodes = nodes[1:]

	case ftReal:
		// REAL or REAL * 4
		typ.baseType = "float64" // TODO : correct type "float32"
		nodes = nodes[1:]
		if len(nodes) > 1 &&
			nodes[0].tok == token.MUL &&
			nodes[1].tok == token.INT {
			switch string(nodes[1].b) {
			case "4": // REAL or REAL * 4
				typ.baseType = "float32"
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

	// Array:
	//  INTEGER (3)

	if len(nodes) == 0 {
		return
	}

	if nodes[0].tok != token.LPAREN {
		return
	}
	nodes = nodes[1:]

	if nodes[0].tok == token.INT {
		val, err := strconv.Atoi(string(nodes[0].b))
		if err != nil {
			panic(fmt.Errorf(
				"Cannot parse array size on `%s` : %v ", string(nodes[0].b), err))
		}
		typ.arrayType = append(typ.arrayType, val)
	} else {
		typ.arrayType = append(typ.arrayType, -1)
	}
	nodes = nodes[1:]

	if nodes[0].tok == token.COMMA {
		nodes = nodes[1:]
		if nodes[0].tok == token.INT {
			val, err := strconv.Atoi(string(nodes[0].b))
			if err != nil {
				panic(fmt.Errorf(
					"Cannot parse array size on `%s` : %v ", string(nodes[0].b), err))
			}
			typ.arrayType = append(typ.arrayType, val)
		} else {
			typ.arrayType = append(typ.arrayType, -1)
		}
		nodes = nodes[1:]
	}

	if nodes[0].tok != token.RPAREN {
		panic("Unsupport type " + nodesToString(nodes))
	}

	return
}
