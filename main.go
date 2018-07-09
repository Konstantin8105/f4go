package main

import (
	"fmt"
	"os/exec"

	"github.com/Konstantin8105/f4go/ast"
)

func main() {
	// Example:
	// gfortran -fdump-tree-original-raw ./hello.f
	// Result into file: ./hello.f.003t.original
}

func transpile(file string) (err error) {
	treeFile := file + ".out"
	err = exec.Command("gfortran",
		"-fdump-tree-original-raw="+treeFile,
		file).Run()
	if err != nil {
		return
	}
	var nss [][]interface{}
	nss, err = ast.ParseAST(treeFile)
	if err != nil {
		return
	}

	for _, ns := range nss {
		err = trans(ns)
		if err != nil {
			return
		}
	}

	return
}

func trans(ns []interface{}) (err error) {
	if len(ns) < 1 {
		return
	}
	if _, ok := ns[0].(ast.Function_decl); !ok {
		err = fmt.Errorf("Not function_decl : %#v", ns[0])
		return
	}
	n := ns[0].(ast.Function_decl)

	fmt.Printf("%#v\n", n)

	// if index, ok := ast.IsLink(n.Name); ok {
	// 	fmt.Printf("Name = %#v\n", ns[index-1])
	// }
	// if index, ok := ast.IsLink(n.VarType); ok {
	// 	fmt.Printf("VarType = %#v\n", ns[index-1])
	// }
	if index, ok := ast.IsLink(n.Body); ok {
		// fmt.Printf("Body = %#v\n", ns[index-1])

		n := ns[index-1].(ast.Bind_expr)
		// parse var_decl
		if index, ok := ast.IsLink(n.Vars); ok {
			fmt.Printf("Var decl= %#v\n", ns[index-1])
			err = transpileVarDecl(ns[index-1].(ast.Var_decl), ns)
			if err != nil {
				return
			}
		}

	}

	return
}

func transpileVarDecl(n ast.Var_decl, ns []interface{}) (err error) {
	if index, ok := ast.IsLink(n.Name); ok {
		fmt.Printf("Name = %#v\n", ns[index-1])
	}

	if index, ok := ast.IsLink(n.TypeD); ok {
		var t string
		t, err = transpileType(ns[index-1], ns)
		if err != nil {
			return
		}
		fmt.Printf("Type = %v\n", t)
	}

	return
}

func transpileType(n interface{}, ns []interface{}) (t string, err error) {
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
	}
	return
}
