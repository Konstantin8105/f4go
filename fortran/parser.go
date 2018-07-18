package fortran

import (
	goast "go/ast"
)

func Parse(filename string) (ast goast.File, err error) {
	ast.Name = goast.NewIdent("main")
	return
}
