package fortran

// Token represents a lexical token.
type Token int

const (
	// Special tokens
	ILLEGAL Token = iota
	EOF
	WS

	// Literals
	IDENT // fields, table_name

	// Other
	OPEN  // (
	CLOSE // )
	COMMA // ,

	DOUBLE_POINTS // :

	COMMENT // !

	EQUAL // =
	STAR  // *
	NUMBER
)
