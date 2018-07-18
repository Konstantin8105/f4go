package fortran

import (
	"fmt"
	goast "go/ast"
	"go/token"
	"os"
)

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
		if tok == PROGRAM {
			err = parseProgram(sc, &ast)
			if err != nil {
				return
			}
			continue
		}
		_ = lit
	}

	return
}

func parseProgram(sc *Scanner, ast *goast.File) (err error) {
	var fd goast.FuncDecl

	tok, lit := getNextIdent(sc)

	if tok == token.IDENT {
		fd.Name = goast.NewIdent(lit)
	} else {
		err = fmt.Errorf("Cannot found name")
		return
	}

	fd.Type = &goast.FuncType{}
	fd.Body = &goast.BlockStmt{}

	ast.Decls = append(ast.Decls, &fd)
	return
}

func getNextIdent(sc *Scanner) (tok token.Token, lit string) {
next:
	tok, lit = sc.Scan()
	if tok == token.COMMENT || tok == NEW_LINE {
		goto next
	}
	return
}
