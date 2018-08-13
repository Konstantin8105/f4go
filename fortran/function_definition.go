package fortran

import goast "go/ast"

var intrinsicFunction = []struct {
	functionName string
	typeNames    []string
	changeFunc   func(*goast.FuncDecl)
}{
	{
		functionName: "real",
		typeNames:    []string{"float64"},
		changeFunc: func(f *goast.FuncDecl) {
			f.Name = goast.NewIdent("real")
		},
	},
	{
		functionName: "aimag",
		typeNames:    []string{"float64"},
		changeFunc: func(f *goast.FuncDecl) {
			f.Name = goast.NewIdent("imag")
		},
	},
}
