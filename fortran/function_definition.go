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

		if len(id.Name) > 11 && id.Name[:11] == "*func()*int" {
			// *func()*int{y:=6;return &y}()
			id.Name = id.Name[15:]
			id.Name = id.Name[:len(id.Name)-13]
		}
		if len(id.Name) > 8 && id.Name[:8] == "*func()*" {
			// TODO : for other types
			// fmt.Println("Simply : ", id.Name)
		}
	}

	return c
}

type intrinsic struct {
	p *parser
}

func (in intrinsic) Visit(node goast.Node) (w goast.Visitor) {

	if call, ok := node.(*goast.CallExpr); ok {
		if n, ok := call.Fun.(*goast.Ident); ok {
			if f, ok := intrinsicFunction[strings.ToUpper(n.Name)]; ok {
				f(in.p, call)
			} else if n.Name != "make" && n.Name != "append" {
				n.Name = strings.ToUpper(n.Name)
			}
		}
	}
	if call, ok := node.(*goast.CallExpr); ok {
		if sel, ok := call.Fun.(*goast.SelectorExpr); ok {
			if x, ok := sel.X.(*goast.Ident); ok && x.Name == "intrinsic" {

				var isRead bool = sel.Sel.Name == "READ"

				for i := range call.Args {
					if isRead && i > 1 {
						// for READ command other arguments is pointer always
						continue
					}
					if arg, ok := call.Args[i].(*goast.Ident); ok {
						if len(arg.Name) > 3 && arg.Name[:2] == "&(" {
							arg.Name = arg.Name[2 : len(arg.Name)-1]
							continue
						}
						if strings.Contains(arg.Name, "func()*[]byte{y:=[]byte(") {
							arg.Name = arg.Name[17:]
							index := strings.LastIndex(arg.Name, "\")")
							arg.Name = arg.Name[:index+2]
							if i > 1 {
								arg.Name = arg.Name[7 : len(arg.Name)-1]
							}
							continue
						}
						if len(arg.Name) > 10 && arg.Name[:7] == "func()*" {
							arg.Name = "*" + arg.Name
							continue
						}
					}
					if un, ok := call.Args[i].(*goast.UnaryExpr); ok {
						if par, ok := un.X.(*goast.ParenExpr); ok {
							if id, ok := par.X.(*goast.IndexExpr); ok {
								call.Args[i] = id
								continue
							}
						}
					}
				}
			}
		}
	}
	return in
}

var intrinsicFunction = map[string]func(*parser, *goast.CallExpr){
	"REAL": func(p *parser, f *goast.CallExpr) {
		typeNames := []string{"complex128"}
		intrinsicArgumentCorrection(p, f, "real", typeNames)
	},
	"AIMAG": func(p *parser, f *goast.CallExpr) {
		typeNames := []string{"complex128"}
		intrinsicArgumentCorrection(p, f, "imag", typeNames)
	},
	"LEN": func(p *parser, f *goast.CallExpr) {
		typeNames := []string{"[]byte"}
		intrinsicArgumentCorrection(p, f, "len", typeNames)
	},
	"MIN": func(p *parser, f *goast.CallExpr) {
		typeNames := []string{"int", "int"}
		p.addImport("github.com/Konstantin8105/f4go/intrinsic")
		intrinsicArgumentCorrection(p, f, "intrinsic.MIN", typeNames)
	},
}

func intrinsicArgumentCorrection(p *parser, f *goast.CallExpr, name string, typeNames []string) {
	if _, ok := f.Fun.(*goast.Ident); !ok || len(f.Args) != len(typeNames) {
		return
	}
	f.Fun.(*goast.Ident).Name = name

	for i := range typeNames {
		if id, ok := f.Args[i].(*goast.Ident); ok {
			if len(id.Name) > 3 && id.Name[:2] == "&(" {
				id.Name = id.Name[2 : len(id.Name)-1]
				continue
			}
		}
		if un, ok := f.Args[i].(*goast.UnaryExpr); ok {
			if par, ok := un.X.(*goast.ParenExpr); ok {
				if id, ok := par.X.(*goast.IndexExpr); ok {
					f.Args[i] = id
					continue
				}
			}
		}
	}
}
