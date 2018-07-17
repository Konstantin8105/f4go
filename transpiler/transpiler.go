package transpiler

import (
	"bytes"
	"fmt"
	"reflect"
	"strconv"

	goast "go/ast"
	"go/format"
	"go/token"

	"github.com/Konstantin8105/f4go/ast"
)

type transpiler struct {
	ns          []ast.Node
	initVarDecl map[string]bool
}

func (tr *transpiler) isVarDeclInit(name string) bool {
	if tr.initVarDecl == nil {
		tr.initVarDecl = map[string]bool{}
	}
	_, ok := tr.initVarDecl[name]
	// fmt.Println("CHECK : ", name, ok, tr.initVarDecl)
	return ok
}

func (tr *transpiler) addVarDecl(name string) {
	if tr.initVarDecl == nil {
		tr.initVarDecl = map[string]bool{}
	}
	tr.initVarDecl[name] = true
	// fmt.Println("ADD: ", name, tr.initVarDecl)
}

const f4goUndefined string = "f4goUndefined"

func TranspileAST(nss [][]ast.Node) (gocode string, err error) {
	var file goast.File
	file.Name = goast.NewIdent("main")

	for _, ns := range nss {
		if len(ns) == 0 {
			continue
		}
		var fd goast.FuncDecl
		fmt.Printf("\n\n==== NEXT FUNC DECL =====\n\n")

		t := transpiler{
			ns: ns,
		}

		fd, err = t.trans()
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

	gocode = buf.String()

	return
}

func (tr *transpiler) trans() (fd goast.FuncDecl, err error) {
	if len(tr.ns) < 2 {
		return
	}
	if _, ok := tr.ns[1].(ast.Function_decl); !ok {
		err = fmt.Errorf("Not function_decl : %#v", tr.ns[0])
		return
	}
	n := tr.ns[1].(ast.Function_decl)

	// function name
	if index, ok := ast.IsLink(n.Name); ok {
		var name string
		name, err = tr.getName(tr.ns[index], index)
		if err != nil {
			return
		}
		fd.Name = goast.NewIdent(name)
	} else {
		panic("1")
	}

	// funciton type
	tr.reflectClarification(n)
	if index, ok := ast.IsLink(n.Type); ok {
		fmt.Printf("Cannot TODO : VarType = %#v\n", tr.ns[index])
		tr.reflectClarification(tr.ns[index])

		n := tr.ns[index].(ast.Function_type)

		// add Return type
		var typ string
		if index, ok := ast.IsLink(n.Retn); ok {
			typ, err = tr.transpileType(tr.ns[index])
			if err != nil {
				err = nil
				typ = "UndefineVarType"
			}
		}

		if typ != "void" {
			typ, err = castToGoType(typ)
			if err != nil {
				return
			}
			fd.Type = &goast.FuncType{
				Results: &goast.FieldList{
					List: []*goast.Field{
						&goast.Field{
							Type: goast.NewIdent(typ),
						},
					},
				},
			}
		} else {
			fd.Type = &goast.FuncType{}
		}
		// end of return  type

		var param []*goast.Field
		next := n.Prms

	AGAIN:
		if index, ok := ast.IsLink(next); ok {
			var field *goast.Field
			field, next, err = tr.transpileField(tr.ns[index])
			if err != nil {
				return
			}
			if field != (*goast.Field)(nil) {
				param = append(param, field)
			}
			if next != "" {
				goto AGAIN
			}
		}

		fd.Type.Params = &goast.FieldList{}
		fd.Type.Params.List = param

	} else {
		fmt.Println("--- not found var type")
	}

	// function body
	if index, ok := ast.IsLink(n.Body); ok {
		var stmts []goast.Stmt
		stmts, err = tr.transpileStmt(tr.ns[index], index)
		if err != nil {
			return
		}
		// var varsStmts []goast.Stmt
		// varsStmts, err = tr.parseVarDecls()
		// if err != nil {
		// 	return
		// }
		// stmts = append(varsStmts, stmts...)
		fd.Body = &goast.BlockStmt{
			Lbrace: 1,
			List:   stmts,
			Rbrace: 1,
		}
	}

	return
}

// func (tr *transpiler) parseVarDecls() (stmts []goast.Stmt, err error) {
// 	for i := range tr.ns {
// 		if vd, ok := tr.ns[i].(ast.Var_decl); ok {
// 			var decl goast.Decl
// 			decl, err = tr.transpileVarDecl(vd, i)
// 			if err != nil {
// 				return
// 			}
//
// 			if decl != nil {
// 				stmts = append(stmts, &goast.DeclStmt{Decl: decl})
// 			} else {
// 				fmt.Println("Decl is null")
// 			}
// 		}
// 	}
// 	return
// }

func isNull(ds []goast.Stmt) bool {
	for i := range ds {
		if ds[i] == nil {
			return true
		}
	}
	return false
}

func (tr *transpiler) transpileField(n ast.Node) (
	field *goast.Field, next string, err error) {

	switch n := n.(type) {

	case ast.Tree_list:
		next = n.Chan
		if index, ok := ast.IsLink(n.Valu); ok {
			field, _, err = tr.transpileField(tr.ns[index])
			if err != nil {
				return
			}
		}

	default:
		fmt.Printf("Cannot transpileField : %#v\n", n)
		tr.reflectClarification(n)
	}
	return
}

const tempVarF4GO string = "tempVarF4GO"

func (tr *transpiler) transpileVarDecl(n ast.Var_decl, position int) (
	decl goast.Decl, err error) {

	var t, name string

	if n.Name == "" {
		name = tempVarF4GO + "_" + strconv.Itoa(position)
	} else {
		if index, ok := ast.IsLink(n.Name); ok {
			name = tr.ns[index].(ast.Identifier_node).Strg
		}
	}

	if index, ok := ast.IsLink(n.Type); ok {
		t, err = tr.transpileType(tr.ns[index])
		if err != nil {
			return
		}
		t, err = castToGoType(t)
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
	tr.addVarDecl(name)
	decl = &genDecl
	return
}

func (tr *transpiler) getName(n ast.Node, position int) (name string, err error) {
	// fmt.Printf("getName: %#v\n", n)

	switch n := n.(type) {
	case ast.Identifier_node:
		name = n.Strg
	case ast.Var_decl:
		name = n.Name
		if n.Name == "" {
			name = tempVarF4GO + "_" + strconv.Itoa(position)
		}
	case ast.Integer_cst:
		name = n.Int
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
			name, err = tr.getName(tr.ns[index], index)
			if err != nil {
				return
			}
		}
		var location string
		if index, ok := ast.IsLink(n.Op1); ok {
			location, err = tr.getName(tr.ns[index], index)
			if err != nil {
				return
			}
		}
		name = fmt.Sprintf("%s[%s]", name, location)
	default:
		fmt.Printf("Name is not found: %#v\n", n)
		name = f4goUndefined + "GetNameFunction" + n.GenNodeName()
	}
	// fmt.Println("name = ", name)
	if index, ok := ast.IsLink(name); ok {
		// fmt.Println(">>> ", index, ok)
		return tr.getName(tr.ns[index], index)
	}
	return
}

func (tr *transpiler) transpileDecl(n ast.Node) (name, t string, err error) {
	switch n := n.(type) {
	// case ast.Var_decl:
	// 	name, t, err = tr.transpileVarDecl(n, tr.ns)
	case ast.Integer_cst:
		name = n.Int

		if index, ok := ast.IsLink(n.Type); ok {
			t, err = tr.transpileType(tr.ns[index])
			if err != nil {
				return
			}
		}
		t, err = castToGoType(t)
		if err != nil {
			return
		}

	default:
		fmt.Printf("Cannot transpileDecl: %#v\n", n)
	}
	return
}

func (tr *transpiler) arithOperation(n0, n1 string, tk token.Token) (
	expr goast.Expr, err error) {

	var left, right goast.Expr

	if index, ok := ast.IsLink(n0); ok {
		left, err = tr.transpileExpr(tr.ns[index], index)
		if err != nil {
			// return
			err = nil // ignore
		}
	}

	if index, ok := ast.IsLink(n1); ok {
		right, err = tr.transpileExpr(tr.ns[index], index)
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

func (tr *transpiler) transpileExpr(n ast.Node, position int) (
	expr goast.Expr, err error) {
	switch n := n.(type) {

	case ast.Nop_expr:
		fmt.Println("Cannot TODO: * or &")
		if index, ok := ast.IsLink(n.Op0); ok {
			expr, err = tr.transpileExpr(tr.ns[index], index)
			if err != nil {
				return
			}
		}

	case ast.Integer_cst:
		var name string
		name, err = tr.getName(n, position)
		if err != nil {
			return
		}
		expr = goast.NewIdent(name)

	case ast.Parm_decl:
		var name string
		name, err = tr.getName(n, position)
		if err != nil {
			return
		}
		expr = goast.NewIdent(name)

	case ast.Addr_expr:
		var name string
		if index, ok := ast.IsLink(n.Op0); ok {
			name, err = tr.getName(tr.ns[index], index)
			if err != nil {
				return
			}
		}
		expr = goast.NewIdent(name)

	case ast.Component_ref:
		var base goast.Expr
		var field string
		if index, ok := ast.IsLink(n.Op0); ok {
			base, err = tr.transpileExpr(tr.ns[index], index)
			if err != nil {
				return
			}
		}
		if index, ok := ast.IsLink(n.Op1); ok {
			field, err = tr.getName(tr.ns[index], index)
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
			name, err = tr.getName(tr.ns[index], index)
			if err != nil {
				return
			}
		}

		// fmt.Printf("Cannot TODO : Call <%s> %#v\n", name, n)
		// tr.reflectCarification(n, tr.ns)

		var call goast.CallExpr
		call.Fun = goast.NewIdent(name)
		call.Lparen = 1
		call.Rparen = 1

		for _, v := range n.Vals {
			// fmt.Printf("Vals -> %#v\n", v)
			if index, ok := ast.IsLink(v); ok {
				// fmt.Printf("\t%#v\n", tr.ns[index])
				var e goast.Expr
				e, err = tr.transpileExpr(tr.ns[index], index)
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
		name, err = tr.getName(n, position)
		if err != nil {
			return
		}
		if name == "" {
			name = tempVarF4GO + "_" + strconv.Itoa(position)
		}

		expr = goast.NewIdent(name)

	case ast.Minus_expr:
		expr, err = tr.arithOperation(n.Op0, n.Op1, token.SUB)

	case ast.Plus_expr:
		expr, err = tr.arithOperation(n.Op0, n.Op1, token.ADD)

	case ast.Mult_expr:
		expr, err = tr.arithOperation(n.Op0, n.Op1, token.MUL)

	case ast.Trunc_div_expr:
		expr, err = tr.arithOperation(n.Op0, n.Op1, token.QUO)

	default:
		fmt.Printf("Cannot transpileExpr: %#v\n", n)
		tr.reflectClarification(n)
		expr = goast.NewIdent(f4goUndefined + n.GenNodeName())
	}
	return
}

func (tr *transpiler) transpileStmt(n ast.Node, position int) (
	decls []goast.Stmt, err error) {

	switch n := n.(type) {

	case ast.Return_expr:
		var expr goast.Expr
		if index, ok := ast.IsLink(n.Expr); ok {
			expr, err = tr.transpileExpr(tr.ns[index], index)
			if err != nil {
				return
			}
			if expr == nil {
				fmt.Printf("ReturnStmt is nil. %#v\n", tr.ns[index])
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
		expr, err = tr.transpileExpr(n, position)
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
			// fmt.Printf("Var decl= %#v\n", tr.ns[index])

			var decl goast.Decl
			decl, err = tr.transpileVarDecl(tr.ns[index].(ast.Var_decl), index)
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

			// fmt.Printf("Expr = %#v\n", tr.ns[index])
			var ds []goast.Stmt
			ds, err = tr.transpileStmt(tr.ns[index], index)
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
	// 	expr, err = tr.transpileExpr(n, tr.ns)
	// 	if err != nil {
	// 		return
	// 	}
	// 	decl = append(decl, &goast.ExprStmt{expr})

	case ast.Modify_expr:
		// fmt.Printf("%#v\n", n)

		var left goast.Expr
		if index, ok := ast.IsLink(n.Op0); ok {
			if vd, ok := tr.ns[index].(ast.Var_decl); ok {
				var name string
				name, err = tr.getName(vd, index)
				if err != nil {
					return
				}
				if !tr.isVarDeclInit(name) {
					var decl goast.Decl
					decl, err = tr.transpileVarDecl(vd, index)
					if err != nil {
						return
					}
					decls = append(decls, &goast.DeclStmt{Decl: decl})
				}
			}
			left, err = tr.transpileExpr(tr.ns[index], index)
			if err != nil {
				// return
				err = nil // ignore
			}
		}

		var right goast.Expr
		if index, ok := ast.IsLink(n.Op1); ok {
			right, err = tr.transpileExpr(tr.ns[index], index)
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
		decls = append(decls, &goast.AssignStmt{
			Lhs: []goast.Expr{left},
			Tok: token.ASSIGN,
			Rhs: []goast.Expr{right},
		})

	case ast.Statement_list:

		for i := range n.Vals {
			if index, ok := ast.IsLink(n.Vals[i]); ok {
				var decl []goast.Stmt
				decl, err = tr.transpileStmt(tr.ns[index], index)
				if err != nil {
					return
				}
				decls = append(decls, decl...)
			}
		}

	default:
		fmt.Printf("Cannot transpileStmt: %#v\n", n)
		tr.reflectClarification(n)
	}
	return
}

// reflect clarification
func (tr *transpiler) reflectClarification(n ast.Node) {
	defer func() {
		if r := recover(); r != nil {
			fmt.Println("recover : ", r)
		}
	}()
	if n == nil {
		fmt.Println("Cannot reflectClarification : ", n)
		return
	}
	fmt.Printf("Reflect clarification for : %#v\n", n)
	val := reflect.Indirect(reflect.ValueOf(n))
	for i := 0; i < val.NumField(); i++ {
		f := val.Field(i)
		if index, ok := ast.IsLink(f.String()); ok {
			fmt.Printf("\t%d: %s\t--> %#v\n", i, f.String(), tr.ns[index])
		} else {
			fmt.Printf("\t%d: %#v\n", i, f.Interface())
		}
	}
}

func (tr *transpiler) transpileType(n ast.Node) (t string, err error) {
	switch n := n.(type) {
	case ast.Integer_type:
		if index, ok := ast.IsLink(n.Name); ok {
			t, err = tr.transpileType(tr.ns[index])
			if err != nil {
				return
			}
		}

	case ast.Type_decl:
		if index, ok := ast.IsLink(n.Name); ok {
			t, err = tr.transpileType(tr.ns[index])
			if err != nil {
				return
			}
		}

	case ast.Real_type:
		if index, ok := ast.IsLink(n.Name); ok {
			t, err = tr.transpileType(tr.ns[index])
			if err != nil {
				return
			}
		}

	case ast.Record_type:
		if index, ok := ast.IsLink(n.Name); ok {
			t, err = tr.transpileType(tr.ns[index])
			if err != nil {
				return
			}
		}

	case ast.Void_type:
		if index, ok := ast.IsLink(n.Name); ok {
			t, err = tr.transpileType(tr.ns[index])
			if err != nil {
				return
			}
		}

	case ast.Array_type:
		var size, typ string
		if index, ok := ast.IsLink(n.Size); ok {
			size, err = tr.getName(tr.ns[index], index)
			if err != nil {
				return
			}
		}
		if index, ok := ast.IsLink(n.Elts); ok {
			typ, err = tr.transpileType(tr.ns[index])
			if err != nil {
				return
			}
		}
		typ, err = castToGoType(typ)
		if err != nil {
			return
		}
		t = fmt.Sprintf("[%v]%v", size, typ)

	case ast.Identifier_node:
		t = n.Strg
	default:
		fmt.Printf("Cannot transpileType : %#v\n", n)
		tr.reflectClarification(n)
	}
	return
}
