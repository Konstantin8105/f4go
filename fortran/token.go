package fortran

import (
	"go/token"
)

const (
	ftDoubleStar token.Token = iota + token.VAR + 10 // **
	ftSubroutine

	ftInteger
	ftCharacter
	ftComplex
	ftLogical
	ftReal
	ftData

	ftExternal
	ftDoubleColon
	ftImplicit
	ftFunction
	ftEnd
	ftElseif
	ftDo
	ftDimension
	ftDouble
	ftCall
	ftThen
	ftNewLine

	ftWrite
	ftWhile

	ftParameter
	ftPrecision
	ftProgram
	ftIntrinsic
	ftFormat
	ftStop
	ftDollar

	ftStringConcat
	ftSave
	ftOpen
	ftRead
	ftAssign
	ftDefine
	ftClose
	ftEquivalence
	ftCommon
	ftRewind

	// undefine tokens
	ftUndefine
)

func view(t token.Token) string {
	if int(t) < int(token.VAR)+1 {
		return t.String()
	}
	return o[t]
}

var o = [...]string{
	ftSubroutine: "SUBROUTINE",

	ftInteger:   "INTEGER",
	ftCharacter: "CHARACTER",
	ftComplex:   "COMPLEX",
	ftLogical:   "LOGICAL",
	ftReal:      "REAL",
	ftData:      "DATA",

	ftExternal:    "EXTERNAL",
	ftDoubleColon: "DOUBLE_COLON",
	ftImplicit:    "IMPLICIT",
	ftFunction:    "FUNCTION",
	ftEnd:         "END",
	ftElseif:      "ELSEIF",
	ftDo:          "DO",
	ftDimension:   "DIMENSION",
	ftDouble:      "DOUBLE",
	ftCall:        "CALL",
	ftThen:        "THEN",
	ftNewLine:     "NEW_LINE",

	ftWrite: "WRITE",
	ftWhile: "WHILE",

	ftParameter: "PARAMETER",
	ftPrecision: "PRECISION",
	ftProgram:   "PROGRAM",
	ftIntrinsic: "INTRINSIC",
	ftFormat:    "FORMAT",
	ftStop:      "STOP",

	ftStringConcat: "STRING_CONCAT",
	ftSave:         "SAVE",
	ftOpen:         "OPEN",
	ftRead:         "READ",
	ftAssign:       "ASSIGN",
	ftDefine:       "DEFINE",
	ftClose:        "CLOSE",

	ftDollar: "$",

	ftEquivalence: "EQUIVALENCE",
	ftCommon:      "COMMON",
	ftRewind:      "REWIND",

	ftUndefine: "UNDEFINE",
}
