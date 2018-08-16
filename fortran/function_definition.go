package fortran

import (
	goast "go/ast"
	"strings"
)

type callArgumentSimplification struct {
}

func (c callArgumentSimplification) Visit(node goast.Node) (w goast.Visitor) {
	// from : &((*a))
	// to   : a
	if id, ok := node.(*goast.Ident); ok {
		if len(id.Name) > 6 && id.Name[:4] == "&((*" {
			id.Name = id.Name[4 : len(id.Name)-2]
		}
	}
	return c
}

type intrinsic struct {
}

func (in intrinsic) Visit(node goast.Node) (w goast.Visitor) {
	if call, ok := node.(*goast.CallExpr); ok {
		if n, ok := call.Fun.(*goast.Ident); ok {
			if f, ok := intrinsicFunction[strings.ToUpper(n.Name)]; ok {
				f(call)
			}
		}
	}
	if call, ok := node.(*goast.CallExpr); ok {
		if sel, ok := call.Fun.(*goast.SelectorExpr); ok {
			if x, ok := sel.X.(*goast.Ident); ok && x.Name == "intrinsic" {

				for i := range call.Args {
					if _, ok := call.Args[i].(*goast.Ident); !ok {
						continue
					}
					arg := call.Args[i].(*goast.Ident)
					if len(arg.Name) > 3 && arg.Name[:2] == "&(" {
						arg.Name = arg.Name[2 : len(arg.Name)-1]
					}
					if len(arg.Name) > 15 && arg.Name[:14] == " func()*[]byte" {
						arg.Name = "*" + arg.Name
					}
				}
			}
		}
	}
	return in
}

var intrinsicFunction = map[string]func(*goast.CallExpr){
	"REAL": func(f *goast.CallExpr) {
		typeNames := []string{"complex128"}
		intrinsicArgumentCorrection(f, "real", typeNames)
	},
	"AIMAG": func(f *goast.CallExpr) {
		typeNames := []string{"complex128"}
		intrinsicArgumentCorrection(f, "imag", typeNames)
	},
}

func intrinsicArgumentCorrection(f *goast.CallExpr, name string, typeNames []string) {
	if _, ok := f.Fun.(*goast.Ident); !ok || len(f.Args) != len(typeNames) {
		return
	}
	f.Fun.(*goast.Ident).Name = name

	for i := range typeNames {
		if id, ok := f.Args[i].(*goast.Ident); ok {
			if len(id.Name) > 3 && id.Name[:2] == "&(" {
				id.Name = id.Name[2 : len(id.Name)-1]
			}
		}
	}
}
