package fortran

import (
	"fmt"
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
		err = fmt.Errorf("Cannot found name. : %s %s", view(tok), lit)
		return
	}

	// body
	for {
		tok, _ := sc.Scan()
		if tok == END {
			break
		}
		if tok == token.EOF {
			err = fmt.Errorf("Not acceptable EOF")
			return
		}
	}
	for {
		tok, _ := sc.Scan()
		if tok == NEW_LINE {
			break
		}
		if tok == token.EOF {
			err = fmt.Errorf("Not acceptable EOF")
			return
		}
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
