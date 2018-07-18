package fortran

import "go/token"

const (
	DOUBLE_STAR token.Token = iota + token.VAR + 10 // **
	SUBROUTINE
	PROGRAM
	INTEGER
	DOUBLE_COLON
	IMPLICIT
	FUNCTION
	END
	DO
	ENDDO
)
