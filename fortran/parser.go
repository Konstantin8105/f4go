package fortran

import (
	goast "go/ast"
	"go/token"
	"os"
)

type parser struct {
	sc  *Scanner
	buf struct {
		tok token.Token
		lit string
		n   int // buffer
	}
}

func Parse(filename string) (ast goast.File, err error) {
	ast.Name = goast.NewIdent("main")

	file, err := os.Open(filename)
	if err != nil {
		return
	}
	defer file.Close()

	sc := NewScanner(file)

	for {
		tok, lit := sc.Scan()
		if tok == token.EOF {
			break
		}
		_ = lit
	}

	return
}
