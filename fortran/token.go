package fortran

import (
	"go/token"
)

const (
	DOUBLE_STAR token.Token = iota + token.VAR + 10 // **
	SUBROUTINE

	INTEGER
	CHARACTER
	COMPLEX
	LOGICAL
	REAL
	DATA

	EXTERNAL
	DOUBLE_COLON
	IMPLICIT
	FUNCTION
	END
	ELSEIF
	DO
	DOUBLE
	CALL
	THEN
	NEW_LINE

	WRITE
	WHILE

	PARAMETER
	PRECISION
	PROGRAM
	INTRINSIC
	FORMAT
	STOP
	DOLLAR

	STRING_CONCAT

	// undefine tokens
	undefine
)

func view(t token.Token) string {
	if int(t) < int(token.VAR)+1 {
		return t.String()
	}
	return o[t]
}

var o = [...]string{
	SUBROUTINE: "SUBROUTINE",

	INTEGER:   "INTEGER",
	CHARACTER: "CHARACTER",
	COMPLEX:   "COMPLEX",
	LOGICAL:   "LOGICAL",
	REAL:      "REAL",
	DATA:      "DATA",

	EXTERNAL:     "EXTERNAL",
	DOUBLE_COLON: "DOUBLE_COLON",
	IMPLICIT:     "IMPLICIT",
	FUNCTION:     "FUNCTION",
	END:          "END",
	ELSEIF:       "ELSEIF",
	DO:           "DO",
	DOUBLE:       "DOUBLE",
	CALL:         "CALL",
	THEN:         "THEN",
	NEW_LINE:     "NEW_LINE",

	WRITE: "WRITE",
	WHILE: "WHILE",

	PARAMETER: "PARAMETER",
	PRECISION: "PRECISION",
	PROGRAM:   "PROGRAM",
	INTRINSIC: "INTRINSIC",
	FORMAT:    "FORMAT",
	STOP:      "STOP",

	STRING_CONCAT: "STRING_CONCAT",

	DOLLAR: "$",

	undefine: "UNDEFINE",
}
