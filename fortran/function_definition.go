package fortran

import goast "go/ast"

type ifunc struct {
	typeNames  []string
	changeFunc func(*goast.FuncDecl)
}

var intrinsicFunction = map[string]ifunc{
	"real": ifunc{
		typeNames: []string{"float64"},
		changeFunc: func(f *goast.FuncDecl) {
			f.Name = goast.NewIdent("real")
		},
	},
	"aimag": ifunc{
		typeNames: []string{"float64"},
		changeFunc: func(f *goast.FuncDecl) {
			f.Name = goast.NewIdent("imag")
		},
	},
}
