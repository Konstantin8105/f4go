package fortran

import (
	goast "go/ast"
	"go/token"
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
	if un, ok := node.(*goast.UnaryExpr); ok && un.Op == token.AND {
		if par1, ok := un.X.(*goast.ParenExpr); ok {
			if par2, ok := par1.X.(*goast.ParenExpr); ok {
				if st, ok := par2.X.(*goast.StarExpr); ok {
					node = st.X
				}
			}
		}
	}
	return c
}

type intrinsic struct {
}

func (in intrinsic) Visit(node goast.Node) (w goast.Visitor) {
	if call, ok := node.(*goast.CallExpr); ok {
		if n, ok := call.Fun.(*goast.Ident); ok {
			if f, ok := intrinsicFunction[n.Name]; ok {
				f(call)
			}
		}
	}
	return in
}

var intrinsicFunction = map[string]func(*goast.CallExpr){
	"real": func(f *goast.CallExpr) {
		typeNames := []string{"complex128"}
		intrinsicArgumentCorrection(f, "real", typeNames)
	},
	"aimag": func(f *goast.CallExpr) {
		typeNames := []string{"complex128"}
		intrinsicArgumentCorrection(f, "imag", typeNames)
	},
}

func intrinsicArgumentCorrection(f *goast.CallExpr, name string, typeNames []string) {
	if len(f.Args) != len(typeNames) {
		return
	}
	if _, ok := f.Fun.(*goast.Ident); !ok {
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
