package transpiler

import (
	"bytes"
	"fmt"
	"reflect"

	goast "go/ast"
	"go/format"
	"go/token"

	"github.com/Konstantin8105/f4go/ast"
)

const f4goUndefined string = "f4goUndefined"

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
	fmt.Printf("Code:\n%v", buf.String())

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
		fmt.Printf("Cannot TODO : VarType = %#v\n", ns[index-1])

		fd.Type = &goast.FuncType{}

	} else {
		fmt.Println("--- not found var type")
	}

	// function body
	if index, ok := ast.IsLink(n.Body); ok {
		var stmts []goast.Stmt
		stmts, err = transpileStmt(ns[index-1], ns)
		if err != nil {
			return
		}
		fd.Body = &goast.BlockStmt{
			Lbrace: 1,
			List:   stmts,
			Rbrace: 1,
		}
	}

	return
}

func isNull(ds []goast.Stmt) bool {
	for i := range ds {
		if ds[i] == nil {
			return true
		}
	}
	return false
}

func CastToGoType(fortranType string) (goType string, err error) {
	switch fortranType {
	case "integer(kind=4)":
		goType = "int"
	case "real(kind=4)":
		goType = "float64"
	case "character(kind=1)":
		goType = "byte"
	default:
		fmt.Printf("Cannot CastToGoType: %v\n", fortranType)
		goType = fortranType
	}
	return
}

func transpileVarDecl(n ast.Var_decl, ns []ast.Node) (decl goast.Decl, err error) {

	var t, name string

	if index, ok := ast.IsLink(n.Name); ok {
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
	}

	if t == "" {
		t = f4goUndefined + "Type" + n.GenNodeName()
	}
	if name == "" {
		name = f4goUndefined + "Name" + n.GenNodeName()
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
	// fmt.Printf("getName: %#v\n", n)
	switch n := n.(type) {
	case ast.Identifier_node:
		name = n.Strg
	case ast.Var_decl:
		name = n.Name
	case ast.Integer_cst:
		name = n.VarInt
	case ast.Addr_expr:
		name = n.Op0
	case ast.Function_decl:
		name = n.Name
	case ast.Parm_decl:
		name = n.Name
	case ast.String_cst:
		name = "\"" + n.Strg + "\""
	case ast.Field_decl:
		name = n.Name
	case ast.Array_ref:
		if index, ok := ast.IsLink(n.Op0); ok {
			name, err = getName(ns[index-1], ns)
			if err != nil {
				return
			}
		}
		var location string
		if index, ok := ast.IsLink(n.Op1); ok {
			location, err = getName(ns[index-1], ns)
			if err != nil {
				return
			}
		}
		name = fmt.Sprintf("%s[%s]", name, location)
	default:
		fmt.Printf("Name is not found: %#v\n", n)
		name = f4goUndefined + "GetNameFunction"
	}
	// fmt.Println("name = ", name)
	if index, ok := ast.IsLink(name); ok {
		// fmt.Println(">>> ", index, ok)
		return getName(ns[index-1], ns)
	}
	return
}

func transpileDecl(n ast.Node, ns []ast.Node) (name, t string, err error) {
	switch n := n.(type) {
	// case ast.Var_decl:
	// 	name, t, err = transpileVarDecl(n, ns)
	case ast.Integer_cst:
		name = n.VarInt

		if index, ok := ast.IsLink(n.VarType); ok {
			t, err = transpileType(ns[index-1], ns)
			if err != nil {
				return
			}
		}
		t, err = CastToGoType(t)
		if err != nil {
			return
		}

	default:
		fmt.Printf("Cannot transpileDecl: %#v\n", n)
	}
	return
}

func arithOperation(n0, n1 string, tk token.Token, ns []ast.Node) (
	expr goast.Expr, err error) {

	var left, right goast.Expr

	if index, ok := ast.IsLink(n0); ok {
		left, err = transpileExpr(ns[index-1], ns)
		if err != nil {
			// return
			err = nil // ignore
		}
	}

	if index, ok := ast.IsLink(n1); ok {
		right, err = transpileExpr(ns[index-1], ns)
		if err != nil {
			// return
			err = nil // ignore
		}
	}

	if left == nil {
		left = goast.NewIdent(f4goUndefined + "L")
	}
	if right == nil {
		right = goast.NewIdent(f4goUndefined + "R")
	}

	expr = &goast.BinaryExpr{
		X:  left,
		Op: tk,
		Y:  right,
	}

	return
}

func transpileExpr(n ast.Node, ns []ast.Node) (
	expr goast.Expr, err error) {
	switch n := n.(type) {

	case ast.Integer_cst:
		var name string
		name, err = getName(n, ns)
		if err != nil {
			return
		}
		expr = goast.NewIdent(name)

	case ast.Parm_decl:
		var name string
		name, err = getName(n, ns)
		if err != nil {
			return
		}
		expr = goast.NewIdent(name)

	case ast.Addr_expr:
		var name string
		if index, ok := ast.IsLink(n.Op0); ok {
			name, err = getName(ns[index-1], ns)
			if err != nil {
				return
			}
		}
		expr = goast.NewIdent(name)

	case ast.Component_ref:
		var base goast.Expr
		var field string
		if index, ok := ast.IsLink(n.Op0); ok {
			base, err = transpileExpr(ns[index-1], ns)
			if err != nil {
				return
			}
		}
		if index, ok := ast.IsLink(n.Op1); ok {
			field, err = getName(ns[index-1], ns)
			if err != nil {
				return
			}
		}

		expr = &goast.SelectorExpr{
			X:   base,
			Sel: goast.NewIdent(field),
		}

	case ast.Call_expr:
		var name string
		if index, ok := ast.IsLink(n.Fn); ok {
			name, err = getName(ns[index-1], ns)
			if err != nil {
				return
			}
		}

		// fmt.Printf("Cannot TODO : Call <%s> %#v\n", name, n)
		// reflectClarification(n, ns)

		var call goast.CallExpr
		call.Fun = goast.NewIdent(name)
		call.Lparen = 1
		call.Rparen = 1

		for _, v := range n.Vals {
			// fmt.Printf("Vals -> %#v\n", v)
			if index, ok := ast.IsLink(v); ok {
				// fmt.Printf("\t%#v\n", ns[index-1])
				var e goast.Expr
				e, err = transpileExpr(ns[index-1], ns)
				if err != nil {
					// return
					err = nil // ignore
				}
				if e == nil {
					e = goast.NewIdent(f4goUndefined + "DefaultArgExpr")
				}
				call.Args = append(call.Args, e)
			} else {
				call.Args = append(call.Args,
					goast.NewIdent(f4goUndefined+"DefaultArg"))
			}
		}

		//TODO: need type and arguments

		expr = &call

	case ast.Real_cst:
		expr = goast.NewIdent(n.Valu)

	case ast.Var_decl:
		var name string
		name, err = getName(n, ns)
		if err != nil {
			return
		}
		if name == "" {
			name = f4goUndefined + n.GenNodeName()
		}
		expr = goast.NewIdent(name)

	case ast.Plus_expr:
		expr, err = arithOperation(n.Op0, n.Op1, token.ADD, ns)

	case ast.Mult_expr:
		expr, err = arithOperation(n.Op0, n.Op1, token.MUL, ns)

	case ast.Trunc_div_expr:
		expr, err = arithOperation(n.Op0, n.Op1, token.QUO, ns)

	default:
		fmt.Printf("Cannot transpileExpr: %#v\n", n)
		reflectClarification(n, ns)
		expr = goast.NewIdent(f4goUndefined + n.GenNodeName())
	}
	return
}

func transpileStmt(n ast.Node, ns []ast.Node) (
	decls []goast.Stmt, err error) {

	switch n := n.(type) {

	case ast.Return_expr:
		var expr goast.Expr
		if index, ok := ast.IsLink(n.Expr); ok {
			expr, err = transpileExpr(ns[index-1], ns)
			if err != nil {
				return
			}
			if expr == nil {
				fmt.Printf("ReturnStmt is nil. %#v\n", ns[index-1])
			}
		}
		if expr == nil {
			fmt.Printf("ReturnStmt is nil\n")
			expr = goast.NewIdent(f4goUndefined + n.GenNodeName())
		}
		decls = append(decls, &goast.ReturnStmt{
			Results: []goast.Expr{expr},
		})

	case ast.Call_expr:
		var expr goast.Expr
		expr, err = transpileExpr(n, ns)
		if err != nil {
			return
		}

		decls = append(decls, &goast.ExprStmt{X: expr})

	case ast.Bind_expr:
		var b goast.BlockStmt
		b.Rbrace = 1
		b.Lbrace = 1

		// parse var_decl
		// fmt.Println("-----------")
		if index, ok := ast.IsLink(n.Vars); ok {
			// fmt.Printf("Var decl= %#v\n", ns[index-1])

			var decl goast.Decl
			decl, err = transpileVarDecl(ns[index-1].(ast.Var_decl), ns)
			if err != nil {
				return
			}

			if decl != nil {
				b.List = append(b.List, &goast.DeclStmt{Decl: decl})
			} else {
				fmt.Println("Decl is null")
			}

			// TODO : need all initialization
		}
		// fmt.Println("++++++++++++")

		// parse body
		// fmt.Println("-----------")
		if index, ok := ast.IsLink(n.Body); ok {

			// fmt.Printf("Expr = %#v\n", ns[index-1])
			var ds []goast.Stmt
			ds, err = transpileStmt(ns[index-1], ns)
			if err != nil {
				return
			}
			if isNull(ds) {
				fmt.Println("Decl is null")
			} else {
				b.List = append(b.List, ds...)
			}
		}
		// fmt.Println("++++++++++++")

		decls = append(decls, &b)

	// case ast.Modify_expr:
	// 	var expr goast.Expr
	// 	expr, err = transpileExpr(n, ns)
	// 	if err != nil {
	// 		return
	// 	}
	// 	decl = append(decl, &goast.ExprStmt{expr})

	case ast.Modify_expr:
		// fmt.Printf("%#v\n", n)

		var left goast.Expr
		if index, ok := ast.IsLink(n.Op0); ok {
			left, err = transpileExpr(ns[index-1], ns)
			if err != nil {
				// return
				err = nil // ignore
			}
		}

		var right goast.Expr
		if index, ok := ast.IsLink(n.Op1); ok {
			right, err = transpileExpr(ns[index-1], ns)
			if err != nil {
				// return
				err = nil // ignore
			}
		}
		// fmt.Println("Modify_expr: ", left, " = ", right)

		if left == nil {
			fmt.Println("left is null")
			left = goast.NewIdent(f4goUndefined + "L" + n.GenNodeName())
		}
		if right == nil {
			fmt.Println("right is null")
			right = goast.NewIdent(f4goUndefined + "R" + n.GenNodeName())
		}
		decl := &goast.AssignStmt{
			Lhs: []goast.Expr{left},
			Tok: token.ASSIGN,
			Rhs: []goast.Expr{right},
		}
		decls = append(decls, decl)

	case ast.Statement_list:

		for i := range n.Vals {
			if index, ok := ast.IsLink(n.Vals[i]); ok {
				var decl []goast.Stmt
				decl, err = transpileStmt(ns[index-1], ns)
				if err != nil {
					return
				}
				decls = append(decls, decl...)
			}
		}

	default:
		fmt.Printf("Cannot transpileStmt: %#v\n", n)
		reflectClarification(n, ns)
	}
	return
}

// reflect clarification
func reflectClarification(n ast.Node, ns []ast.Node) {
	val := reflect.Indirect(reflect.ValueOf(n))
	for i := 0; i < val.NumField(); i++ {
		f := val.Field(i)
		if index, ok := ast.IsLink(f.String()); ok {
			fmt.Printf("\t%d: %s\t--> %#v\n", i, f.String(), ns[index-1])
		} else {
			fmt.Printf("\t%d: %#v\n", i, f.Interface())
		}
	}
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

	case ast.Real_type:
		if index, ok := ast.IsLink(n.Name); ok {
			t, err = transpileType(ns[index-1], ns)
			if err != nil {
				return
			}
		}

	case ast.Record_type:
		if index, ok := ast.IsLink(n.Name); ok {
			t, err = transpileType(ns[index-1], ns)
			if err != nil {
				return
			}
		}

	case ast.Array_type:
		var size, typ string
		if index, ok := ast.IsLink(n.Size); ok {
			size, err = getName(ns[index-1], ns)
			if err != nil {
				return
			}
		}
		if index, ok := ast.IsLink(n.Elts); ok {
			typ, err = transpileType(ns[index-1], ns)
			if err != nil {
				return
			}
		}
		typ, err = CastToGoType(typ)
		if err != nil {
			return
		}
		t = fmt.Sprintf("[%v]%v", size, typ)

	case ast.Identifier_node:
		t = n.Strg
	default:
		fmt.Printf("Cannot transpileType : %#v\n", n)
		reflectClarification(n, ns)
	}
	return
}
