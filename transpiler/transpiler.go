package transpiler

import (
	"bytes"
	"fmt"

	goast "go/ast"
	"go/format"
	"go/token"

	"github.com/Konstantin8105/f4go/ast"
)

func TranspileAST(nss [][]ast.Node) (err error) {
	var file goast.File
	file.Name = goast.NewIdent("main")

	for _, ns := range nss {
		if len(ns) == 0 {
			continue
		}
		var fd goast.FuncDecl
		fd, err = trans(ns)
		if err != nil {
			return
		}
		// goast.Print(token.NewFileSet(), fd)
		file.Decls = append(file.Decls, &fd)
	}

	var buf bytes.Buffer
	if err = format.Node(&buf, token.NewFileSet(), &file); err != nil {
		return
	}
	fmt.Println("Code:\n", buf.String())

	return
}

func trans(ns []ast.Node) (fd goast.FuncDecl, err error) {
	if len(ns) < 1 {
		return
	}
	if _, ok := ns[0].(ast.Function_decl); !ok {
		err = fmt.Errorf("Not function_decl : %#v", ns[0])
		return
	}
	n := ns[0].(ast.Function_decl)

	// function name
	if index, ok := ast.IsLink(n.Name); ok {
		var name string
		name, err = getName(ns[index-1], ns)
		if err != nil {
			return
		}
		fd.Name = goast.NewIdent(name)
	} else {
		panic("1")
	}

	// funciton type
	if index, ok := ast.IsLink(n.VarType); ok {
		fmt.Printf("VarType = %#v\n", ns[index-1])

		fd.Type = &goast.FuncType{}

	} else {
		fmt.Println("--- not found var type")
	}

	// function body
	if index, ok := ast.IsLink(n.Body); ok {
		var blockStmt goast.BlockStmt
		blockStmt, err = parseBlock(ns[index-1], ns)
		if err != nil {
			return
		}
		fd.Body = &blockStmt
	}

	return
}

func parseBlock(n ast.Node, ns []ast.Node) (b goast.BlockStmt, err error) {
	fmt.Printf("|| Block : %#v\n", n)

	b.Rbrace = 1
	b.Lbrace = 1

	switch n := n.(type) {
	case ast.Bind_expr:
		// parse var_decl
		fmt.Println("-----------")
		if index, ok := ast.IsLink(n.Vars); ok {
			fmt.Printf("Var decl= %#v\n", ns[index-1])

			var decl goast.Decl
			decl, err = transpileVarDecl(ns[index-1].(ast.Var_decl), ns)
			if err != nil {
				return
			}
			b.List = append(b.List, &goast.DeclStmt{Decl: decl})
			// fmt.Println("Create var: ", n, t)
		}
		fmt.Println("++++++++++++")

		// parse body
		fmt.Println("-----------")
		if index, ok := ast.IsLink(n.Body); ok {

			fmt.Printf("Expr = %#v\n", ns[index-1])
			err = transpileExpr(ns[index-1], ns)
			if err != nil {
				return
			}
		}
		fmt.Println("++++++++++++")
	default:
		err = fmt.Errorf("Cannot parse block : %#v", n)
	}

	return
}

func transpileIntegerCast(n ast.Integer_cst, ns []ast.Node) (name, t string, err error) {
	// if index, ok := ast.IsLink(n.VarInt); ok {
	// 	fmt.Printf("Name = %#v\n", ns[index-1])
	// 	name = ns[index-1].(ast.Identifier_node).Strg
	// }
	name = n.VarInt

	if index, ok := ast.IsLink(n.VarType); ok {
		t, err = transpileType(ns[index-1], ns)
		if err != nil {
			return
		}
		fmt.Printf("Type = %v\n", t)
	}

	return
}

func CastToGoType(fortranType string) (goType string, err error) {
	switch fortranType {
	case "integer(kind=4)":
		goType = "int"
	default:
		fmt.Printf("Cannot CastToGoType: %v\n", fortranType)
	}
	return
}

func transpileVarDecl(n ast.Var_decl, ns []ast.Node) (decl goast.Decl, err error) {

	var t, name string

	if index, ok := ast.IsLink(n.Name); ok {
		fmt.Printf("Name = %#v\n", ns[index-1])
		name = ns[index-1].(ast.Identifier_node).Strg
	}

	if index, ok := ast.IsLink(n.TypeD); ok {
		t, err = transpileType(ns[index-1], ns)
		if err != nil {
			return
		}
		t, err = CastToGoType(t)
		if err != nil {
			return
		}
		fmt.Printf("Type = %v\n", t)
	}

	genDecl := goast.GenDecl{
		Tok: token.VAR,
		Specs: []goast.Spec{
			&goast.ValueSpec{
				Names: []*goast.Ident{{Name: name}},
				Type:  goast.NewIdent(t),
			},
		},
	}
	decl = &genDecl
	return
}

func getName(n ast.Node, ns []ast.Node) (name string, err error) {
	switch n := n.(type) {
	case ast.Identifier_node:
		name = n.Strg
	default:
		err = fmt.Errorf("Type is not found: %#v", n)
	}
	return
}

func transpileDecl(n ast.Node, ns []ast.Node) (name, t string, err error) {
	switch n := n.(type) {
	// case ast.Var_decl:
	// 	name, t, err = transpileVarDecl(n, ns)
	case ast.Integer_cst:
		name, t, err = transpileIntegerCast(n, ns)
	default:
		fmt.Printf("Cannot transpileDecl: %#v\n", n)
	}
	return
}

func transpileExpr(n ast.Node, ns []ast.Node) (err error) {
	switch n := n.(type) {
	case ast.Modify_expr:
		fmt.Printf("%#v\n", n)

		if index, ok := ast.IsLink(n.Op0); ok {
			var name, t string
			name, t, err = transpileDecl(ns[index-1], ns)
			if err != nil {
				return
			}
			fmt.Println("Decl ", name, t)
		}

		if index, ok := ast.IsLink(n.Op1); ok {
			var name, t string
			name, t, err = transpileDecl(ns[index-1], ns)
			if err != nil {
				return
			}
			fmt.Println("Decl ", name, t)
		}

	default:
		fmt.Printf("Cannot transpileExpr : %#v\n", n)
	}
	return
}

func transpileType(n ast.Node, ns []ast.Node) (t string, err error) {
	switch n := n.(type) {
	case ast.Integer_type:
		if index, ok := ast.IsLink(n.Name); ok {
			t, err = transpileType(ns[index-1], ns)
			if err != nil {
				return
			}
		}
	case ast.Type_decl:
		if index, ok := ast.IsLink(n.Name); ok {
			t, err = transpileType(ns[index-1], ns)
			if err != nil {
				return
			}
		}
	case ast.Identifier_node:
		t = n.Strg
	default:
		fmt.Printf("Cannot transpileType : %#v\n", n)
	}
	return
}
