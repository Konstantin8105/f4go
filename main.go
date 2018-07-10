package main

import (
	"os/exec"

	"github.com/Konstantin8105/f4go/ast"
	"github.com/Konstantin8105/f4go/transpiler"
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
	var nss [][]ast.Node
	nss, err = ast.ParseAST(treeFile)
	if err != nil {
		return
	}

	err = transpiler.TranspileAST(nss)
	if err != nil {
		return
	}

	return
}
