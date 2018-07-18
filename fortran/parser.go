package fortran

import (
	"fmt"
	goast "go/ast"
	"go/token"
	"os"
)

type node struct {
	tok token.Token
	lit string
}

func Parse(filename string) (ast goast.File, err error) {
	ast.Name = goast.NewIdent("main")

	file, err := os.Open(filename)
	if err != nil {
		return
	}
	defer file.Close()

	sc := NewScanner(file)

	var nodes []node

	for {
		tok, lit := sc.Scan()
		if tok == token.EOF {
			break
		}

		switch tok {
		case token.COMMENT, NEW_LINE:
			continue
		}
		nodes = append(nodes, node{
			tok: tok,
			lit: lit,
		})
		fmt.Println(lit)
	}

	return
}
