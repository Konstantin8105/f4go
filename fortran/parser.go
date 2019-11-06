package fortran

import (
	"bytes"
	"fmt"
	goast "go/ast"
	goparser "go/parser"
	"go/token"
	"os"
	"runtime/debug"
	"strconv"
	"strings"
)

type varInitialization struct {
	name string
	typ  goType
}

type varInits []varInitialization

func (v varInits) get(n string) (varInitialization, bool) {
	n = strings.ToUpper(n)
	for _, val := range []varInitialization(v) {
		if val.name == n {
			return val, true
		}
	}
	return varInitialization{}, false
}

func (v *varInits) del(n string) {
	vs := []varInitialization(*v)
	n = strings.ToUpper(n)
	for i, val := range vs {
		if val.name == n {
			vs = append(vs[:i], vs[i+1:]...)
			*v = varInits(vs)
			return
		}
	}
}

func (v *varInits) add(name string, typ goType) {
	vs := []varInitialization(*v)
	vs = append(vs, varInitialization{name: strings.ToUpper(name), typ: typ})
	*v = varInits(vs)
}

func (p parser) getSize(name string, col int) (size int, ok bool) {
	v, ok := p.initVars.get(name)
	if !ok {
		panic("Cannot find variable : " + name)
	}
	if v.typ.baseType == "string" {
		col++
	}
	if len(v.typ.arrayNode[col]) == 1 && v.typ.arrayNode[col][0].tok == token.INT {
		val, _ := strconv.Atoi(string(v.typ.arrayNode[col][0].b))
		return val, true
	}
	if vv, ok := p.initVars.get(nodesToString(v.typ.arrayNode[col])); ok {
		if n, ok := p.constants[vv.name]; ok {
			size, _ = strconv.Atoi(nodesToString(n))
			return size, true
		}
	}
	for i, n := range v.typ.arrayNode[col] {
		// Example:
		// -1 : 1
		// Nodes:
		// [[-, `-`] [INT, `1`] [:, `:`] [INT, `1`]]
		// 99 : 101
		// Nodes:
		// [[INT, `99`] [:, `:`] [INT, `101`]]
		if n.tok == token.COLON {
			begin, err := strconv.Atoi(strings.Replace(nodesToString(v.typ.arrayNode[col][:i]), " ", "", -1))
			if err != nil {
				p.addError("Cannot parse begin value : " + nodesToString(v.typ.arrayNode[col][:i]))
				break
			}
			end, err := strconv.Atoi(strings.Replace(nodesToString(v.typ.arrayNode[col][i+1:]), " ", "", -1))
			if err != nil {
				p.addError("Cannot parse end value : " + nodesToString(v.typ.arrayNode[col][i+1:]))
				break
			}
			return end - begin + 1, true
		}
	}
	return -1, false
}

func (p parser) getArrayBegin(name string, col int) int {
	v, ok := p.initVars.get(name)
	if !ok {
		panic("Cannot find variable : " + name)
	}
	if v.typ.baseType == "string" {
		col++
	}
	if col >= len(v.typ.arrayNode) {
		return 1
	}
	for i, n := range v.typ.arrayNode[col] {
		// Example:
		// -1 : 1
		// Nodes:
		// [[-, `-`] [INT, `1`] [:, `:`] [INT, `1`]]
		// 99 : 101
		// Nodes:
		// [[INT, `99`] [:, `:`] [INT, `101`]]
		if n.tok == token.COLON {
			strBegin := strings.Replace(nodesToString(v.typ.arrayNode[col][:i]), " ", "", -1)
			b, err := strconv.Atoi(strBegin)
			if err != nil {
				p.addError("Cannot parse begin value: " + strBegin)
			}
			return b
		}
	}
	return 1
}

func (p parser) getArrayLen(name string) int {
	v, ok := p.initVars.get(name)
	if !ok {
		panic("Cannot find variable : " + name)
	}
	lenArray := len(v.typ.arrayNode)
	if v.typ.baseType == "string" {
		lenArray--
	}
	return lenArray
}

type common struct {
	mem map[string][]varInitialization
}

func (c *common) addBlockName(name string, vars []varInitialization) {
	if c.mem == nil {
		c.mem = map[string][]varInitialization{}
	}
	// common may add new vars
	if v, ok := c.mem[name]; ok {
		for i := range v {
			found := false
			for j := range vars {
				if v[i].name == vars[j].name {
					found = true
				}
			}
			if found {
				continue
			}
			vars = append(vars, v[i])
		}
	}
	c.mem[name] = vars
}

type implicitVariable struct {
	symbol byte
	typ    []node
}

type parser struct {
	ast   goast.File
	ident int
	ns    []node

	Common common // share memory between subroutines

	implicit []implicitVariable

	functionExternalName []string

	initVars varInits // map of name to type

	comments []string

	pkgs        map[string]bool // import packages
	endLabelDo  map[string]int  // label of DO
	allLabels   map[string]bool // list of all labels
	foundLabels map[string]bool // list labels found in source

	parameters map[string]string // constants

	formats map[string][]node // source line with command FORMAT

	constants map[string][]node

	errs []error
}

func (p *parser) addImport(pkg string) {
	p.pkgs[pkg] = true
}

func (p *parser) init() {
	p.functionExternalName = make([]string, 0)
	p.endLabelDo = map[string]int{}
	p.allLabels = map[string]bool{}
	p.foundLabels = map[string]bool{}
	p.initVars = varInits{}
	p.parameters = map[string]string{}
	p.formats = map[string][]node{}
	p.implicit = nil
	p.constants = map[string][]node{}
}

// list view - only for debugging
func lv(ns []node) (output string) {
	for _, n := range ns {
		b := string(n.b)
		if n.tok != ftNewLine {
			output += fmt.Sprintf("%10s\t%10s\t|`%s`\n",
				view(n.tok),
				fmt.Sprintf("%v", n.pos),
				b)
		} else {
			output += fmt.Sprintf("%20s\n",
				view(n.tok))
		}
	}
	return
}

// Parse is convert fortran source to go ast tree
func Parse(b []byte, packageName string) (_ goast.File, errs []error) {

	if packageName == "" {
		packageName = "main"
	}

	var p parser

	if p.pkgs == nil {
		p.pkgs = map[string]bool{}
	}

	p.ns = scan(b)

	p.ast.Name = goast.NewIdent(packageName)

	var decls []goast.Decl
	p.ident = 0
	decls = p.parseNodes()

	// add packages
	for pkg := range p.pkgs {
		p.ast.Decls = append(p.ast.Decls, &goast.GenDecl{
			Tok: token.IMPORT,
			Specs: []goast.Spec{
				&goast.ImportSpec{
					Path: &goast.BasicLit{
						Kind:  token.STRING,
						Value: "\"" + pkg + "\"",
					},
				},
			},
		})
	}

	// TODO : add INTRINSIC fortran functions

	// put COMMON
	if len(p.Common.mem) > 0 {
		var fields []*goast.Field
		for names, vars := range p.Common.mem {
			var nameFields []*goast.Field
			for i := range vars {
				var (
					varName = vars[i].name
					varType = []byte(vars[i].typ.String())
				)
				// from:
				//
				// [3]int
				// [1]float64
				// [2][2][2]float64
				// float64
				// int
				//
				// to:
				//
				// []int
				// []float64
				// [][][]float64
				// float64
				// int
				isopen := false
				for i := range varType {
					if varType[i] == '[' {
						isopen = true
						continue
					}
					if varType[i] == ']' {
						isopen = false
						continue
					}
					if !isopen {
						continue
					}
					varType[i] = ' '
				}
				nameFields = append(nameFields, &goast.Field{
					Names: []*goast.Ident{goast.NewIdent(varName)},
					Type:  goast.NewIdent(string(varType)),
				})
			}
			fields = append(fields, &goast.Field{
				Names: []*goast.Ident{goast.NewIdent(names)},
				Type:  &goast.StructType{Fields: &goast.FieldList{List: nameFields}},
			})
		}

		p.ast.Decls = append(p.ast.Decls, &goast.GenDecl{
			Tok: token.TYPE,
			Specs: []goast.Spec{
				&goast.TypeSpec{
					Name: goast.NewIdent("MEMORY"),
					Type: &goast.StructType{Fields: &goast.FieldList{List: fields}},
				},
			},
		})

		p.ast.Decls = append(p.ast.Decls, &goast.GenDecl{
			Tok: token.VAR,
			Specs: []goast.Spec{
				&goast.ValueSpec{
					Names: []*goast.Ident{goast.NewIdent("COMMON")},
					Type:  goast.NewIdent("MEMORY"),
				},
			},
		})
	}

	p.ast.Decls = append(p.ast.Decls, decls...)

	strC := strChanger{}
	goast.Walk(strC, &p.ast)

	return p.ast, p.errs
}

// go/ast Visitor for comment label
type commentLabel struct {
	labels map[string]bool
}

func (c commentLabel) Visit(node goast.Node) (w goast.Visitor) {
	if ident, ok := node.(*goast.Ident); ok && ident != nil {
		if _, ok := c.labels[ident.Name]; ok {
			ident.Name = "//" + ident.Name
		}
	}
	return c
}

// go/ast Visitor for change "strings" to "[]byte"
type strChanger struct {
}

func (s strChanger) Visit(node goast.Node) (w goast.Visitor) {
	if call, ok := node.(*goast.CallExpr); ok {
		if sel, ok := call.Fun.(*goast.SelectorExpr); ok {
			if id, ok := sel.X.(*goast.Ident); ok {
				if id.Name == "fmt" || id.Name == "math" {
					return nil
				}
			}
		}
		if ident, ok := call.Fun.(*goast.Ident); ok {
			if ident.Name == "panic" {
				return nil
			}
		}
	}
	if _, ok := node.(*goast.ImportSpec); ok {
		return nil
	}
	if st, ok := node.(*goast.BasicLit); ok && st.Kind == token.STRING {
		if len(st.Value) == 3 {
			st.Kind = token.CHAR
			st.Value = fmt.Sprintf("'%c'", st.Value[1])
		} else {
			st.Value = fmt.Sprintf("*func()*[]byte{y:=[]byte(%s);return &y}()",
				st.Value)
		}
	}
	return s
}

// parseNodes
func (p *parser) parseNodes() (decls []goast.Decl) {
	if Debug {
		fmt.Fprintf(os.Stdout, "Parse nodes\n")
	}

	if p.ident < 0 || p.ident >= len(p.ns) {
		p.errs = append(p.errs,
			fmt.Errorf("Ident is outside nodes: %d/%d", p.ident, len(p.ns)))
		return
	}

	// find all names of FUNCTION, SUBROUTINE, PROGRAM
	var internalFunction []string
	for ; p.ident < len(p.ns); p.ident++ {
		switch p.ns[p.ident].tok {
		case ftSubroutine:
			p.expect(ftSubroutine)
			p.ident++
			p.expect(token.IDENT)
			internalFunction = append(internalFunction, string(p.ns[p.ident].b))
			continue
		case ftProgram:
			p.expect(ftProgram)
			p.ident++
			p.expect(token.IDENT)
			internalFunction = append(internalFunction, string(p.ns[p.ident].b))
			continue
		}

		// Example:
		//   RECURSIVE SUBROUTINE CGELQT3( M, N, A, LDA, T, LDT, INFO )
		if strings.ToUpper(string(p.ns[p.ident].b)) == "RECURSIVE" {
			p.ns[p.ident].tok, p.ns[p.ident].b = ftNewLine, []byte("\n")
			continue
		}

		// FUNCTION
		for i := p.ident; i < len(p.ns) && p.ns[i].tok != ftNewLine; i++ {
			if p.ns[p.ident].tok == ftFunction {
				p.expect(ftFunction)
				p.ident++
				p.expect(token.IDENT)
				internalFunction = append(internalFunction, string(p.ns[p.ident].b))
			}
		}
	}
	p.ident = 0

	for ; p.ident < len(p.ns); p.ident++ {
		p.init()
		p.functionExternalName = append(p.functionExternalName,
			internalFunction...)

		var next bool
		switch p.ns[p.ident].tok {
		case ftDefine:
			p.addError("Cannot parse #DEFINE: " + p.getLine())
			p.gotoEndLine()
			continue

		case ftNewLine:
			next = true // TODO
		case token.COMMENT:
			p.comments = append(p.comments,
				"//"+string(p.ns[p.ident].b))
			next = true // TODO
		case ftSubroutine: // SUBROUTINE
			var decl goast.Decl
			decl = p.parseSubroutine()
			decls = append(decls, decl)
			next = true
		case ftProgram: // PROGRAM
			var decl goast.Decl
			decl = p.parseProgram()
			decls = append(decls, decl)
			next = true
		default:
			// Example :
			//  COMPLEX FUNCTION CDOTU ( N , CX , INCX , CY , INCY )
			for i := p.ident; i < len(p.ns) && p.ns[i].tok != ftNewLine; i++ {
				if p.ns[i].tok == ftFunction {
					decl := p.parseFunction()
					decls = append(decls, decl)
					next = true
				}
			}
		}
		if next {
			continue
		}

		if p.ident >= len(p.ns) {
			break
		}

		switch p.ns[p.ident].tok {
		case ftNewLine, token.EOF:
			continue
		}

		// if at the begin we haven't SUBROUTINE , FUNCTION,...
		// then add fake Program
		var comb []node
		comb = append(comb, p.ns[:p.ident]...)
		comb = append(comb, []node{
			{tok: ftNewLine, b: []byte("\n")},
			{tok: ftProgram, b: []byte("PROGRAM")},
			{tok: token.IDENT, b: []byte("MAIN")},
			{tok: ftNewLine, b: []byte("\n")},
		}...)
		comb = append(comb, p.ns[p.ident:]...)
		p.ns = comb
		p.ident--

		fmt.Fprintf(os.Stdout, "Add fake PROGRAM MAIN in pos : %v", p.ns[p.ident].pos)
	}

	return
}

func (p *parser) gotoEndLine() {
	for ; p.ident < len(p.ns) && p.ns[p.ident].tok != ftNewLine; p.ident++ {
	}
}

func (p *parser) getLine() (line string) {
	if p.ident < 0 {
		p.ident = 0
	}
	if !(p.ident < len(p.ns)) {
		p.ident = len(p.ns) - 1
	}

	last := p.ident
	defer func() {
		p.ident = last
	}()
	for ; p.ident >= 0 && p.ns[p.ident].tok != ftNewLine; p.ident-- {
	}
	p.ident++
	for ; p.ident < len(p.ns) && p.ns[p.ident].tok != ftNewLine; p.ident++ {
		line += " " + string(p.ns[p.ident].b)
	}
	return
}

// go/ast Visitor for parse FUNCTION
type vis struct {
	// map [from] to
	c map[string]string
}

func initVis() *vis {
	var v vis
	v.c = map[string]string{}
	return &v
}

func (v vis) Visit(node goast.Node) (w goast.Visitor) {
	if ident, ok := node.(*goast.Ident); ok {
		if to, ok := v.c[strings.ToUpper(ident.Name)]; ok {
			ident.Name = "(" + to + ")"
		}
	}
	return v
}

// delete external function type definition
func (p *parser) removeExternalFunction() {
	for _, f := range p.functionExternalName {
		p.initVars.del(f)
	}
}

// add correct type of subroutine arguments
func (p *parser) argumentCorrection(fd goast.FuncDecl) (removedVars []string) {
checkArguments:
	for i := range fd.Type.Params.List {
		fieldName := fd.Type.Params.List[i].Names[0].Name
		if v, ok := p.initVars.get(fieldName); ok {
			fd.Type.Params.List[i].Type = goast.NewIdent(v.typ.String())

			// Remove to arg
			removedVars = append(removedVars, fieldName)
			p.initVars.del(fieldName)
			goto checkArguments
		}
	}
	return
}

type replacer struct {
	from, to string
}

func (r replacer) Visit(node goast.Node) (w goast.Visitor) {
	if node != nil {
		if ident, ok := node.(*goast.Ident); ok {
			node.(*goast.Ident).Name = strings.Replace(ident.Name, r.from, r.to, -1)
		}
	}
	return r
}

// init vars
func (p *parser) initializeVars() (vars []goast.Stmt) {
	defer func() {
		if r := recover(); r != nil {
			err := fmt.Sprintf("Recover initializeVars : %v", r)
			if Debug {
				fmt.Fprintf(os.Stdout, "%s\n", err)
			}
			p.addError(err)
			p.gotoEndLine()
		}
	}()
	for i := range []varInitialization(p.initVars) {
		name := ([]varInitialization(p.initVars)[i]).name
		goT := ([]varInitialization(p.initVars)[i]).typ
		switch p.getArrayLen(name) {
		case 0:
			vars = append(vars, &goast.AssignStmt{
				Lhs: []goast.Expr{goast.NewIdent(name)},
				Tok: token.DEFINE,
				Rhs: []goast.Expr{
					&goast.CallExpr{
						Fun: goast.NewIdent("new"),
						Args: []goast.Expr{
							goast.NewIdent(goT.String()),
						},
					},
				},
			})

		case 1: // vector

			fset := token.NewFileSet() // positions are relative to fset
			src := `package main
func main() {
	MATRIX := func() (*[]%s){ arr:=make([]%s, %d); return &arr}()
}
`
			var (
				subName  = "MATRIX"
				size0, _ = p.getSize(name, 0)
				typ      = goT.getBaseType()
			)
			s := fmt.Sprintf(src, typ, typ, size0)
			f, err := goparser.ParseFile(fset, "", s, 0)
			if err != nil {
				panic(fmt.Errorf("Error: %v\nSource:\n%s\npos=%s",
					err, s, goT.arrayNode))
			}
			var r replacer
			r.from = subName
			r.to = name
			goast.Walk(r, f)

			list := f.Decls[0].(*goast.FuncDecl).Body.List
			if strings.Contains(name, "COMMON.") {
				list[0].(*goast.AssignStmt).Tok = token.ASSIGN
			}

			vars = append(vars, list...)

		case 2: // matrix

			fset := token.NewFileSet() // positions are relative to fset
			src := `package main
func main() {
	MATRIX := func()(*[][]%s){
		arr := make([][]%s, %d)
		for u := 0; u < %d; u++ {
			arr[u] = make([]%s, %d)
		}
		return &arr
	}()
}
`
			var (
				subName  = "MATRIX"
				size0, _ = p.getSize(name, 0)
				size1, _ = p.getSize(name, 1)
				typ      = goT.getBaseType()
			)
			s := fmt.Sprintf(src, typ, typ, size0, size0, typ, size1)
			f, err := goparser.ParseFile(fset, "", s, 0)
			if err != nil {
				panic(fmt.Errorf("Error: %v\nSource:\n%s\npos=%s",
					err, s, goT.arrayNode))
			}
			var r replacer
			r.from = subName
			r.to = name
			goast.Walk(r, f)

			list := f.Decls[0].(*goast.FuncDecl).Body.List
			if strings.Contains(name, "COMMON.") {
				list[0].(*goast.AssignStmt).Tok = token.ASSIGN
			}

			vars = append(vars, list...)

		case 3: // ()()()
			fset := token.NewFileSet() // positions are relative to fset
			src := `package main
func main() {
	MATRIX := func()(*[][]%s) {
		arr := make([][][]%s, %d)
		for u := 0; u < %d; u++ {
			arr[u] = make([][]%s, %d)
			for w := 0; w < %d; w++ {
				arr[u][w] = make([]%s, %d)
			}
		}
		return &arr
	}()
}
`
			var (
				subName  = "MATRIX"
				size0, _ = p.getSize(name, 0)
				size1, _ = p.getSize(name, 1)
				size2, _ = p.getSize(name, 2)
				typ      = goT.getBaseType()
			)
			s := fmt.Sprintf(src, typ, typ, size0, size0, typ, size1, size1, typ, size2)
			f, err := goparser.ParseFile(fset, "", s, 0)
			if err != nil {
				panic(fmt.Errorf("Error: %v\nSource:\n%s\npos=%s",
					err, s, goT.arrayNode))
			}

			var r replacer
			r.from = subName
			r.to = name
			goast.Walk(r, f)

			list := f.Decls[0].(*goast.FuncDecl).Body.List
			if strings.Contains(name, "COMMON.") {
				list[0].(*goast.AssignStmt).Tok = token.ASSIGN
			}

			vars = append(vars, list...)

		case 4: // ()()()()
			fset := token.NewFileSet() // positions are relative to fset
			src := `package main
func main() {
	MATRIX := make([][][][]%s, %d)
	for u := 0; u < %d; u++ {
		MATRIX[u] = make([][][]%s, %d)
		for w := 0; w < %d; w++ {
			MATRIX[u][w] = make([][]%s, %d)
			for q := 0; q < %d; q++{
				MATRIX[u][w][q] = make([]%s, %d)
			}
		}
	}
}
`
			var (
				subName  = "MATRIX"
				size0, _ = p.getSize(name, 0)
				size1, _ = p.getSize(name, 1)
				size2, _ = p.getSize(name, 2)
				size3, _ = p.getSize(name, 3)
				typ      = goT.getBaseType()
			)
			s := fmt.Sprintf(src,
				typ, size0, size0,
				typ, size1, size1,
				typ, size2, size2,
				typ, size3,
			)
			f, err := goparser.ParseFile(fset, "", s, 0)
			if err != nil {
				panic(fmt.Errorf("Error: %v\nSource:\n%s\npos=%s",
					err, s, goT.arrayNode))
			}

			var r replacer
			r.from = subName
			r.to = name
			goast.Walk(r, f)

			list := f.Decls[0].(*goast.FuncDecl).Body.List
			if strings.Contains(name, "COMMON.") {
				list[0].(*goast.AssignStmt).Tok = token.ASSIGN
			}

			vars = append(vars, list...)

		case 5: // ()()()()()
			fset := token.NewFileSet() // positions are relative to fset
			src := `package main
func main() {
	MATRIX := make([][][][][]%s, %d)
	for u := 0; u < %d; u++ {
		MATRIX[u] = make([][][][]%s, %d)
		for w := 0; w < %d; w++ {
			MATRIX[u][w] = make([][][]%s, %d)
			for q := 0; q < %d; q++{
				MATRIX[u][w][q] = make([][]%s, %d)
				for s := 0; s < %d; s++{
					MATRIX[u][w][q][s] = make([]%s, %d)
				}
			}
		}
	}
}
`
			var (
				subName  = "MATRIX"
				size0, _ = p.getSize(name, 0)
				size1, _ = p.getSize(name, 1)
				size2, _ = p.getSize(name, 2)
				size3, _ = p.getSize(name, 3)
				size4, _ = p.getSize(name, 4)
				typ      = goT.getBaseType()
			)
			s := fmt.Sprintf(src,
				typ, size0, size0,
				typ, size1, size1,
				typ, size2, size2,
				typ, size3, size3,
				typ, size4,
			)
			f, err := goparser.ParseFile(fset, "", s, 0)
			if err != nil {
				panic(fmt.Errorf("Error: %v\nSource:\n%s\npos=%s",
					err, s, goT.arrayNode))
			}

			var r replacer
			r.from = subName
			r.to = name
			goast.Walk(r, f)

			list := f.Decls[0].(*goast.FuncDecl).Body.List
			if strings.Contains(name, "COMMON.") {
				list[0].(*goast.AssignStmt).Tok = token.ASSIGN
			}

			vars = append(vars, list...)

		default:
			panic(fmt.Errorf(
				"not correct amount of array : %v", goT))
		}
	}

	return
}

// go/ast Visitor for comment label
type callArg struct {
	p *parser
}

func isIgnoreCall(call *goast.CallExpr) bool {
	if sel, ok := call.Fun.(*goast.SelectorExpr); ok {
		if name, ok := sel.X.(*goast.Ident); ok {
			switch name.Name {
			case
				"math",
				"real",
				"fmt":
				return true
			}
		}
	}

	if id, ok := call.Fun.(*goast.Ident); ok {
		switch id.Name {
		case "append",
			"panic":
			return true
		}
	}

	return false
}

// Example
//  From :
// ab_min(3, 14)
//  To:
// ab_min(func() *int { y := 3; return &y }(), func() *int { y := 14; return &y }())
func (c callArg) Visit(node goast.Node) goast.Visitor {
	call, ok := node.(*goast.CallExpr)
	if !ok {
		return c
	}
	if call == nil {
		return nil
	}
	if isIgnoreCall(call) {
		return c
	}

	for i := range call.Args {
		switch a := call.Args[i].(type) {
		case *goast.BasicLit:
			switch a.Kind {
			case token.STRING:
				call.Args[i] = goast.NewIdent(
					fmt.Sprintf("func()*[]byte{y:=[]byte(%s);return &y}()", a.Value))
				if len(a.Value) == 3 {
					a.Value = strings.Replace(a.Value, "\"", "'", -1)
					call.Args[i] = goast.NewIdent(
						fmt.Sprintf("func()*byte{y:=byte(%s);return &y}()", a.Value))
				}
			case token.INT:
				call.Args[i] = goast.NewIdent(
					fmt.Sprintf("func()*int{y:=%s;return &y}()", a.Value))
			case token.FLOAT:
				call.Args[i] = goast.NewIdent(
					fmt.Sprintf("func()*float64{y:=%s;return &y}()", a.Value))
			case token.CHAR:
				call.Args[i] = goast.NewIdent(
					fmt.Sprintf("func()*byte{y:=%s;return &y}()", a.Value))
			default:
				panic(fmt.Errorf(
					"Not support basiclit token: %T ", a.Kind))
			}

		case *goast.Ident, *goast.IndexExpr, *goast.ParenExpr:
			// from:  NAME
			// to  : &NAME
			call.Args[i] = &goast.UnaryExpr{
				Op: token.AND,
				X: &goast.ParenExpr{
					Lparen: 1,
					X:      call.Args[i],
				},
			}

		default:
			// TODO:
			// goast.Print(token.NewFileSet(), a)
			// panic(fmt.Errorf("Not support arg call token: %T ", a))
		}
	}

	return c
}

// Example :
//  COMPLEX FUNCTION CDOTU ( N , CX , INCX , CY , INCY )
//  DOUBLE PRECISION FUNCTION DNRM2 ( N , X , INCX )
//  COMPLEX * 16 FUNCTION ZDOTC ( N , ZX , INCX , ZY , INCY )
func (p *parser) parseFunction() (decl goast.Decl) {
	if Debug {
		fmt.Fprintf(os.Stdout, "Parse function\n")
	}
	for i := p.ident; i < len(p.ns) && p.ns[i].tok != ftNewLine; i++ {
		if p.ns[i].tok == ftFunction {
			p.ns[i].tok = ftSubroutine
		}
	}
	return p.parseSubroutine()
}

// Example:
//   PROGRAM MAIN
func (p *parser) parseProgram() (decl goast.Decl) {
	if Debug {
		fmt.Fprintf(os.Stdout, "Parse program\n")
	}
	p.expect(ftProgram)
	p.ns[p.ident].tok = ftSubroutine
	decl = p.parseSubroutine()
	if fd, ok := decl.(*goast.FuncDecl); ok {
		fd.Name.Name = "main"
	}
	return
}

// parseSubroutine  is parsed SUBROUTINE, FUNCTION, PROGRAM
// Example :
//  SUBROUTINE CHBMV ( UPLO , N , K , ALPHA , A , LDA , X , INCX , BETA , Y , INCY )
//  PROGRAM MAIN
//  COMPLEX FUNCTION CDOTU ( N , CX , INCX , CY , INCY )
func (p *parser) parseSubroutine() (decl goast.Decl) {
	if Debug {
		fmt.Fprintf(os.Stdout, "Parse subroutine\n")
	}

	defer func() {
		p.init()
	}()

	var fd goast.FuncDecl
	fd.Type = &goast.FuncType{
		Params: &goast.FieldList{},
	}

	defer func() {
		fd.Doc = &goast.CommentGroup{}
		for _, c := range p.comments {
			fd.Doc.List = append(fd.Doc.List, &goast.Comment{
				Text: c,
			})
		}
		p.comments = []string{}
	}()

	// check return type
	var returnType []node
	for ; p.ns[p.ident].tok != ftSubroutine && p.ns[p.ident].tok != ftNewLine; p.ident++ {
		returnType = append(returnType, p.ns[p.ident])
	}

	p.expect(ftSubroutine)

	p.ident++
	p.expect(token.IDENT)
	name := strings.ToUpper(string(p.ns[p.ident].b))
	fd.Name = goast.NewIdent(name)
	if Debug {
		fmt.Fprintf(os.Stdout, "subroutine name is : %s\n", name)
	}

	// Add return type is exist
	returnName := name + "_RES"
	if len(returnType) > 0 {
		fd.Type.Results = &goast.FieldList{
			List: []*goast.Field{
				{
					Names: []*goast.Ident{goast.NewIdent(returnName)},
					Type:  goast.NewIdent("*" + parseType(returnType).String()),
				},
			},
		}
	}
	defer func() {
		// change function name variable to returnName
		if len(returnType) > 0 {
			v := initVis()
			v.c[name] = returnName
			goast.Walk(v, fd.Body)
		}
	}()

	// Parameters
	p.ident++
	fd.Type.Params.List = p.parseParamDecl()

	p.ident++
	fd.Body = &goast.BlockStmt{
		Lbrace: 1,
		List:   p.parseListStmt(),
	}

	// delete external function type definition
	p.removeExternalFunction()

	// remove from arguments arg with type string
	arrayArguments := map[string]bool{}
	for i := range fd.Type.Params.List {
		fieldName := fd.Type.Params.List[i].Names[0].Name
		if v, ok := p.initVars.get(fieldName); ok {
			if v.typ.isArray() {
				arrayArguments[fieldName] = true
			}
		}
	}

	// add correct type of subroutine arguments
	arguments := p.argumentCorrection(fd)

	// change arguments
	// From:
	//  a
	// To:
	//  *a
	v := initVis()
	for _, arg := range arguments {
		v.c[arg] = arg
	}
	goast.Walk(v, fd.Body)

	// changes arguments in func
	for i := range fd.Type.Params.List {
		switch fd.Type.Params.List[i].Type.(type) {
		case *goast.Ident:
			id := fd.Type.Params.List[i].Type.(*goast.Ident)
			{
				// remove sizes in matrix
				// from:
				// [2][2]int
				// to:
				// [][]int
				isopen := false
				b := []byte(id.Name)
				for i := range b {
					if b[i] == '[' {
						isopen = true
						continue
					}
					if b[i] == ']' {
						isopen = false
						continue
					}
					if !isopen {
						continue
					}
					b[i] = ' '
				}

				id.Name = string(b)
			}

			// add pointer
			id.Name = "*" + id.Name
		default:
			panic(fmt.Errorf("Cannot parse type in fields: %T",
				fd.Type.Params.List[i].Type))
		}
	}

	// replace call argument constants
	c := callArg{p: p}
	goast.Walk(c, fd.Body)

	// init vars
	fd.Body.List = append(p.initializeVars(), fd.Body.List...)

	// remove unused labels
	removedLabels := map[string]bool{}
	for k := range p.allLabels {
		if _, ok := p.foundLabels[k]; !ok {
			removedLabels[k] = true
		}
	}
	cl := commentLabel{labels: removedLabels}
	goast.Walk(cl, fd.Body)

	in := intrinsic{p: p}
	goast.Walk(in, fd.Body)

	var cas callArgumentSimplification
	goast.Walk(cas, fd.Body)

	decl = &fd
	return
}

func (p *parser) addError(msg string) {
	last := p.ident
	defer func() {
		p.ident = last
	}()

	p.errs = append(p.errs, fmt.Errorf("%s", msg))
}

func (p *parser) expect(t token.Token) {
	if t != p.ns[p.ident].tok {
		// Show all errors
		// No need show all error for avoid dublicates
		//	for _, err := range p.errs {
		//		fmt.Println("Error : ", err.Error())
		//	}
		// Panic
		panic(fmt.Errorf("Expect %s, but we have {{%s,%s}}. Pos = %v",
			view(t), view(p.ns[p.ident].tok), string(p.ns[p.ident].b),
			p.ns[p.ident].pos))
	}
}

func (p *parser) parseListStmt() (stmts []goast.Stmt) {
	for p.ident < len(p.ns) {
		// Only for debugging
		// fmt.Println("---------------")
		// for i := 0; i < len(p.ns); i++ {
		// 	if p.ns[i].tok != ftNewLine {
		// 		fmt.Printf("%8v %4d %3d %v %v\n", p.ident == i,
		// 			i, p.ns[i].pos.line, p.ns[i].tok, string(p.ns[i].b))
		// 		continue
		// 	}
		// 	fmt.Printf("%8v %4d %3d %v\n", p.ident == i,
		// 		i, p.ns[i].pos.line, p.ns[i].tok)
		// }

		if p.ns[p.ident].tok == token.COMMENT {
			stmts = append(stmts, &goast.ExprStmt{
				X: goast.NewIdent("//" + string(p.ns[p.ident].b)),
			})
			p.ident++
			continue
		}
		if p.ns[p.ident].tok == ftNewLine {
			p.ident++
			continue
		}

		if p.ns[p.ident].tok == ftEnd {
			p.ident++
			p.gotoEndLine()
			// TODO need gotoEndLine() ??
			break
		}
		if p.ns[p.ident].tok == token.ELSE {
			// gotoEndLine() is no need for case:
			// ELSE IF (...)...
			break
		}

		stmt := p.parseStmt()
		if len(stmt) == 0 {
			// p.addError("stmt is nil in line ")
			// break
			continue
		}
		stmts = append(stmts, stmt...)
	}
	return
}

// Examples:
//  INTEGER INCX , INCY , N
//  COMPLEX CX ( * ) , CY ( * )
//  COMPLEX*16 A(LDA,*),X(*)
//  REAL A(LDA,*),B(LDB,*)
//  DOUBLE PRECISION DX(*)
//  LOGICAL CONJA,CONJB,NOTA,NOTB
//  CHARACTER*32 SRNAME
func (p *parser) parseInit() (stmts []goast.Stmt) {

	// parse base type
	var baseType []node
	for ; p.ns[p.ident].tok != token.IDENT; p.ident++ {
		baseType = append(baseType, p.ns[p.ident])
	}
	p.expect(token.IDENT)

	var name string
	var additionType []node
	for ; p.ns[p.ident].tok != ftNewLine &&
		p.ns[p.ident].tok != token.EOF; p.ident++ {
		// parse name
		p.expect(token.IDENT)
		name = string(p.ns[p.ident].b)

		// parse addition type
		additionType = []node{}
		p.ident++
		for ; p.ns[p.ident].tok != ftNewLine &&
			p.ns[p.ident].tok != token.EOF &&
			p.ns[p.ident].tok != token.COMMA; p.ident++ {
			if p.ns[p.ident].tok == token.LPAREN {
				counter := 0
				for ; ; p.ident++ {
					switch p.ns[p.ident].tok {
					case token.LPAREN:
						counter++
					case token.RPAREN:
						counter--
					case ftNewLine:
						p.addError("Cannot parse type : not expected NEW_LINE")
						return
					}
					if counter == 0 {
						break
					}
					additionType = append(additionType, p.ns[p.ident])
				}
			}
			additionType = append(additionType, p.ns[p.ident])
		}

		// parse type = base type + addition type
		p.initVars.add(name, parseType(append(baseType, additionType...)))
		if p.ns[p.ident].tok != token.COMMA {
			p.ident--
		}
	}

	return
}

func (p *parser) parseDoWhile() (sDo goast.ForStmt) {
	p.expect(ftDo)
	p.ident++
	p.expect(ftWhile)
	p.ident++
	start := p.ident
	for ; p.ident < len(p.ns); p.ident++ {
		if p.ns[p.ident].tok == ftNewLine {
			break
		}
	}
	sDo.Cond = p.parseExpr(start, p.ident)

	p.expect(ftNewLine)
	p.ident++

	sDo.Body = &goast.BlockStmt{
		Lbrace: 1,
		List:   p.parseListStmt(),
	}

	return
}

func (p *parser) parseDo() (sDo goast.ForStmt) {
	p.expect(ftDo)
	p.ident++
	if p.ns[p.ident].tok == ftWhile {
		p.ident--
		return p.parseDoWhile()
	}
	// possible label
	if p.ns[p.ident].tok == token.INT {
		p.endLabelDo[string(p.ns[p.ident].b)]++
		p.ident++
	}
	// for case with comma "DO 40, J = 1, N"
	if p.ns[p.ident].tok == token.COMMA {
		p.ident++
	}

	p.expect(token.IDENT)
	name := p.ns[p.ident]

	p.ident++
	p.expect(token.ASSIGN)

	p.ident++
	// Init is expression
	start := p.ident
	counter := 0
	for ; p.ident < len(p.ns); p.ident++ {
		if p.ns[p.ident].tok == token.LPAREN {
			counter++
			continue
		}
		if p.ns[p.ident].tok == token.RPAREN {
			counter--
			continue
		}
		if p.ns[p.ident].tok == token.COMMA && counter == 0 {
			break
		}
	}
	sDo.Init = &goast.AssignStmt{
		Lhs: []goast.Expr{
			p.parseExprNodes([]node{name}),
		},
		Tok: token.ASSIGN, // =
		Rhs: []goast.Expr{
			p.parseExpr(start, p.ident),
		},
	}

	p.expect(token.COMMA)

	// Cond is expression
	p.ident++
	start = p.ident
	p.ns = append(p.ns[:start], append([]node{
		name,
		{tok: token.LSS, b: []byte{'<'}},
	}, p.ns[start:]...)...)
	counter = 0
	for ; p.ident < len(p.ns); p.ident++ {
		if p.ns[p.ident].tok == token.LPAREN {
			counter++
			continue
		}
		if p.ns[p.ident].tok == token.RPAREN {
			counter--
			continue
		}
		if (p.ns[p.ident].tok == token.COMMA || p.ns[p.ident].tok == ftNewLine) &&
			counter == 0 {
			break
		}
	}
	sDo.Cond = p.parseBinary(start, p.ident)

	if p.ns[p.ident].tok == ftNewLine {
		sDo.Post = &goast.IncDecStmt{
			X:   p.parseExprNodes([]node{name}),
			Tok: token.INC,
		}
	} else {
		p.expect(token.COMMA)
		p.ident++

		// Post is expression
		start = p.ident
		for ; p.ident < len(p.ns); p.ident++ {
			if p.ns[p.ident].tok == ftNewLine {
				break
			}
		}
		sDo.Post = &goast.AssignStmt{
			Lhs: []goast.Expr{p.parseExprNodes([]node{name})},
			Tok: token.ADD_ASSIGN, // +=
			Rhs: []goast.Expr{p.parseExpr(start, p.ident)},
		}
	}

	p.expect(ftNewLine)

	sDo.Body = &goast.BlockStmt{
		Lbrace: 1,
		List:   p.parseListStmt(),
	}

	return
}

func (p *parser) parseBinary(start, finish int) (expr goast.Expr) {

	if p.ns[start].tok == token.NOT {
		return &goast.UnaryExpr{
			Op: token.NOT,
			X:  &goast.ParenExpr{X: p.parseExpr(start+1, finish)},
		}
	}

	// conditions
	var operationPos int
	for operationPos = start; operationPos < finish; operationPos++ {
		var found bool
		switch p.ns[operationPos].tok {
		case
			token.LAND, // &&
			token.LOR:  // ||
			found = true
		}
		if found {
			left := p.parseBinary(start, operationPos)
			rigth := p.parseBinary(operationPos+1, finish)
			return &goast.BinaryExpr{
				X:  left,
				Op: p.ns[operationPos].tok,
				Y:  rigth,
			}
		}
	}

	for operationPos = start; operationPos < finish; operationPos++ {
		var found bool
		switch p.ns[operationPos].tok {
		case
			token.LSS, // <
			token.GTR, // >
			token.LEQ, // <=
			token.GEQ, // >=
			token.NEQ, // !=
			token.EQL: // ==
			found = true
		}
		if found {
			break
		}
	}
	if start < operationPos && operationPos < finish {
		expr = &goast.BinaryExpr{
			X:  p.parseExpr(start, operationPos),
			Op: p.ns[operationPos].tok,
			Y:  p.parseExpr(operationPos+1, finish),
		}
	} else {
		expr = p.parseExpr(start, finish)
	}
	if b, ok := expr.(*goast.BinaryExpr); ok {
		if _, ok := b.X.(*goast.CallExpr); ok {
			b.X = &goast.ParenExpr{X: &goast.StarExpr{X: b.X}}
		}
		if _, ok := b.Y.(*goast.CallExpr); ok {
			b.Y = &goast.ParenExpr{X: &goast.StarExpr{X: b.Y}}
		}
	}
	return
}

func (p *parser) parseIf() (sIf goast.IfStmt) {
	p.ident++
	p.expect(token.LPAREN)

	p.ident++
	start := p.ident
	for counter := 1; p.ns[p.ident].tok != token.EOF; p.ident++ {
		var exit bool
		switch p.ns[p.ident].tok {
		case token.LPAREN:
			counter++
		case token.RPAREN:
			counter--
			if counter == 0 {
				exit = true
			}
		}
		if exit {
			break
		}
	}

	sIf.Cond = p.parseBinary(start, p.ident)

	p.expect(token.RPAREN)
	p.ident++

	if p.ns[p.ident].tok == ftThen {
		p.gotoEndLine()
		p.ident++
		sIf.Body = &goast.BlockStmt{
			Lbrace: 1,
			List:   p.parseListStmt(),
		}
	} else {
		sIf.Body = &goast.BlockStmt{
			Lbrace: 1,
			List:   p.parseStmt(),
		}
		return
	}

	if p.ident >= len(p.ns) {
		return
	}

	if p.ns[p.ident].tok == token.ELSE {
		p.ident++
		if p.ns[p.ident].tok == token.IF {
			ifr := p.parseIf()
			sIf.Else = &ifr
		} else {
			sIf.Else = &goast.BlockStmt{
				Lbrace: 1,
				List:   p.parseListStmt(),
			}
		}
	}

	return
}

func (p *parser) parseExternal() {
	p.expect(ftExternal)

	p.ident++
	for ; p.ns[p.ident].tok != token.EOF; p.ident++ {
		if p.ns[p.ident].tok == ftNewLine {
			p.ident++
			break
		}
		switch p.ns[p.ident].tok {
		case token.IDENT, ftInteger, ftReal, ftComplex:
			name := string(p.ns[p.ident].b)
			p.functionExternalName = append(p.functionExternalName, name)
		case token.COMMA:
			// ingore
		default:
			p.addError("Cannot parse External " + string(p.ns[p.ident].b))
		}
	}
}

func (p *parser) resetImplicit() {
	p.implicit = nil
}

func (p parser) isImplicit(b byte) (typ []node, ok bool) {
	for i := range p.implicit {
		if b == p.implicit[i].symbol {
			return p.implicit[i].typ, true
		}
	}
	return
}

func (p *parser) parseStmt() (stmts []goast.Stmt) {
	onlyForRecover := p.ident

	pos := p.ns[p.ident].pos

	defer func() {
		if r := recover(); r != nil {
			err := fmt.Sprintf("Recover parseStmt pos{%v}: %v", pos, r)
			if Debug {
				fmt.Fprintf(os.Stdout, "%s\n", err)
			}
			p.addError("stacktrace from panic: \n" + string(debug.Stack()))
			p.addError(err)
			p.gotoEndLine()

			// generate as comment
			stmts = append(stmts, &goast.ExprStmt{
				X: goast.NewIdent("/" + "/" +
					"F4GO: NOT IMPLEMENTED :" +
					nodesToString(p.ns[onlyForRecover:p.ident])),
			})
		}
	}()

	switch p.ns[p.ident].tok {
	case ftInteger, ftCharacter, ftComplex, ftLogical, ftReal, ftDouble:
		stmts = append(stmts, p.parseInit()...)

	case ftEquivalence:
		p.addError(p.getLine())
		p.gotoEndLine()

	case ftRewind:
		s := p.parseRewind()
		stmts = append(stmts, s...)

	case ftDimension:
		// from:
		// DIMENSION M(100)
		// to:
		// INTEGER M(100)
		//
		// from:
		// IMPLICIT REAL M
		// DIMENSION M(100)
		// to:
		// REAL M(100)
		p.expect(ftDimension)
		p.ident++
		p.expect(token.IDENT)
		name := string(p.ns[p.ident].b)
		if typ, ok := p.isImplicit(p.ns[p.ident].b[0]); ok {
			p.ident++
			for ; p.ident < len(p.ns) && p.ns[p.ident].tok != ftNewLine; p.ident++ {
				typ = append(typ, p.ns[p.ident])
			}
			p.initVars.add(name, parseType(typ))
		} else {
			p.ident -= 1
			p.expect(ftDimension)
			p.ns[p.ident].tok = ftInteger
			p.ns[p.ident].b = []byte("INTEGER")
			return p.parseStmt()
		}

	case ftFormat:
		stmts = append(stmts, &goast.ExprStmt{
			X: goast.NewIdent("// Unused by f4go : " + p.getLine()),
		})
		p.gotoEndLine()

	case ftCommon:
		s := p.parseCommon()
		stmts = append(stmts, s...)

	case token.RETURN:
		stmts = append(stmts, &goast.ReturnStmt{})
		p.gotoEndLine()
		p.expect(ftNewLine)

	case ftParameter:
		//  PARAMETER ( ONE = ( 1.0E+0 , 0.0E+0 )  , ZERO = 0.0E+0 )
		s := p.parseParameter()
		stmts = append(stmts, s...)

	case ftOpen:
		s := p.parseOpen()
		stmts = append(stmts, s...)

	case ftRead:
		s := p.parseRead()
		stmts = append(stmts, s...)

	case ftClose:
		s := p.parseClose()
		stmts = append(stmts, s...)

	case ftAssign:
		s := p.parseAssign()
		stmts = append(stmts, s...)

	case ftDefine:
		p.addError("#DEFINE is not support :" + p.getLine())
		p.gotoEndLine()

	case ftSave:
		p.expect(ftSave)
		// ignore command SAVE
		// that command only for optimization
		p.gotoEndLine()

	case ftExternal:
		p.parseExternal()

	case ftNewLine:
		// ignore
		p.ident++

	case token.IF:
		sIf := p.parseIf()
		stmts = append(stmts, &sIf)

	case ftDo:
		sDo := p.parseDo()
		stmts = append(stmts, &sDo)

	case ftCall:
		// Example:
		// CALL XERBLA ( 'CGEMM ' , INFO )
		p.expect(ftCall)
		p.ident++
		start := p.ident
		for ; p.ns[p.ident].tok != ftNewLine; p.ident++ {
		}
		f := p.parseExpr(start, p.ident)
		if p.ident-start == 1 {
			f = &goast.CallExpr{
				Fun: goast.NewIdent(string(p.ns[start].b)),
			}
		}
		stmts = append(stmts, &goast.ExprStmt{
			X: f,
		})
		p.expect(ftNewLine)

	case ftIntrinsic:
		// Example:
		//  INTRINSIC CONJG , MAX
		p.expect(ftIntrinsic)
		p.ns[p.ident].tok = ftExternal
		p.parseExternal()

	case ftData:
		// Example:
		// DATA GAM , GAMSQ , RGAMSQ / 4096.D0 , 16777216.D0 , 5.9604645D-8 /
		sData := p.parseData()
		stmts = append(stmts, sData...)

	case ftWrite:
		sWrite := p.parseWrite()
		stmts = append(stmts, sWrite...)

	case ftStop:
		p.expect(ftStop)
		p.ident++
		var msg []byte
		for ; p.ident < len(p.ns) && p.ns[p.ident].tok != ftNewLine; p.ident++ {
			msg = append(msg, p.ns[p.ident].b...)
		}
		stmts = append(stmts, &goast.ExprStmt{
			X: &goast.CallExpr{
				Fun:    goast.NewIdent("panic"),
				Lparen: 1,
				Args: []goast.Expr{
					&goast.BasicLit{
						Kind:  token.STRING,
						Value: "\"" + string(msg) + "\"",
					},
				},
			},
		})

	case token.GOTO:
		// Examples:
		//  GO TO 30
		//  GO TO ( 40, 80 )IEXC
		// TODO: go to next,(30, 50, 70, 90, 110)
		sGoto := p.parseGoto()
		stmts = append(stmts, sGoto...)
		p.expect(ftNewLine)

	case ftImplicit:
		// Examples:
		//	IMPLICIT DOUBLE PRECISION A
		//	IMPLICIT INTEGER B
		//
		// Only with one symbol name
		p.expect(ftImplicit)
		p.ident++

		var typ []node
		for ; p.ident < len(p.ns); p.ident++ {
			if p.ns[p.ident].tok == ftNewLine || p.ns[p.ident].tok == token.EOF {
				break
			}
			typ = append(typ, p.ns[p.ident])
		}
		impl := implicitVariable{
			symbol: typ[len(typ)-2].b[0],
			typ:    typ[:len(typ)-3],
		}
		p.implicit = append(p.implicit, impl)
		p.expect(ftNewLine)

	case token.INT:
		labelName := string(p.ns[p.ident].b)
		if v, ok := p.endLabelDo[labelName]; ok && v > 0 {
			stmts = append(stmts, p.addLabel(p.ns[p.ident].b))
			// if after END DO, then remove
			for i := p.ident; p.ns[i].tok != ftNewLine; i++ {
				p.ns[i].tok, p.ns[i].b = ftNewLine, []byte("\n")
			}

			// add END DO before that label
			var add []node
			for j := 0; j < v; j++ {
				add = append(add, []node{
					{tok: ftNewLine, b: []byte("\n")},
					{tok: ftEnd, b: []byte("END")},
					{tok: ftNewLine, b: []byte("\n")},
				}...)
			}
			var comb []node
			comb = append(comb, p.ns[:p.ident-1]...)
			comb = append(comb, []node{
				{tok: ftNewLine, b: []byte("\n")},
				{tok: ftNewLine, b: []byte("\n")},
			}...)
			comb = append(comb, add...)
			comb = append(comb, []node{
				{tok: ftNewLine, b: []byte("\n")},
			}...)
			comb = append(comb, p.ns[p.ident-1:]...)
			p.ns = comb
			// remove do labels from map
			p.endLabelDo[labelName] = 0
			return
		}

		if p.ns[p.ident+1].tok == token.CONTINUE {
			stmts = append(stmts, p.addLabel(p.ns[p.ident].b))
			// replace CONTINUE to NEW_LINE
			p.ident++
			p.ns[p.ident].tok, p.ns[p.ident].b = ftNewLine, []byte("\n")
			return
		}

		stmts = append(stmts, p.addLabel(p.ns[p.ident].b))
		p.ident++
		return

	default:
		start := p.ident
		for ; p.ident < len(p.ns); p.ident++ {
			if p.ns[p.ident].tok == ftNewLine {
				break
			}
		}
		var isAssignStmt bool
		pos := start
		if p.ns[start].tok == token.IDENT {
			pos++
			if p.ns[pos].tok == token.LPAREN {
				counter := 0
				for ; pos < len(p.ns); pos++ {
					switch p.ns[pos].tok {
					case token.LPAREN:
						counter++
					case token.RPAREN:
						counter--
					}
					if counter == 0 {
						break
					}
				}
				pos++
			}
			if p.ns[pos].tok == token.ASSIGN {
				isAssignStmt = true
			}
		}

		if isAssignStmt {
			// IMPLICIT initialization
			if v := p.ns[start]; v.tok == token.IDENT {
				_, ok := p.initVars.get(string(v.b))
				if !ok {
					typ, ok := p.isImplicit(v.b[0])
					if ok {
						// add init
						var inject []node
						inject = append(inject, typ...)
						for i := start; i < pos; i++ {
							inject = append(inject, p.ns[i])
						}
						inject = append(inject, node{
							tok: ftNewLine,
							b:   []byte{'\n'},
						})
						p.ns = append(p.ns[:start], append(inject, p.ns[start:]...)...)
						old := p.ident
						p.ident = start
						s := p.parseInit()
						p.ident = old + len(inject)
						if len(s) > 0 {
							stmts = append(stmts, s...)
						}
						start += len(inject)
						pos += len(inject)
					}
				}
			}

			// add assign
			assign := goast.AssignStmt{
				Lhs: []goast.Expr{p.parseExpr(start, pos)},
				Tok: token.ASSIGN, // =
				Rhs: []goast.Expr{p.parseExpr(pos+1, p.ident)},
			}
			if f, ok := assign.Rhs[0].(*goast.CallExpr); ok {
				if !isIgnoreCall(f) {
					assign.Rhs[0] = &goast.ParenExpr{X: &goast.StarExpr{X: assign.Rhs[0]}}
				}
			}
			stmts = append(stmts, &assign)
		} else {
			nodes := p.parseExpr(start, p.ident)
			stmts = append(stmts, &goast.ExprStmt{
				X: nodes,
			})
		}

		p.ident++
	}

	return
}

func (p *parser) addLabel(label []byte) (stmt goast.Stmt) {
	labelName := "Label" + string(label)
	p.allLabels[labelName] = true
	return &goast.LabeledStmt{
		Label: goast.NewIdent(labelName),
		Colon: 1,
		Stmt:  &goast.EmptyStmt{},
	}
}

func (p *parser) parseParamDecl() (fields []*goast.Field) {
	if p.ns[p.ident].tok != token.LPAREN {
		// Function or SUBROUTINE without arguments
		// Example:
		//  SubRoutine CLS
		return
	}
	p.expect(token.LPAREN)

	// Parameters
	p.ident++
	for ; p.ns[p.ident].tok != token.EOF; p.ident++ {
		var exit bool
		switch p.ns[p.ident].tok {
		case token.COMMA:
			// ignore
		case token.IDENT:
			id := strings.ToUpper(string(p.ns[p.ident].b))
			field := &goast.Field{
				Names: []*goast.Ident{goast.NewIdent(id)},
				Type:  goast.NewIdent("int"),
			}
			fields = append(fields, field)
		case token.RPAREN:
			p.ident--
			exit = true
		default:
			p.addError("Cannot parse parameter decl " + string(p.ns[p.ident].b))
			return
		}
		if exit {
			break
		}
	}

	p.ident++
	p.expect(token.RPAREN)

	p.ident++
	p.expect(ftNewLine)

	return
}

// Example:
// ( ( D ( I , J ) , J = 1 , 4 ) , I = 1 , 4 )
// =                             = = = = = = =
//
// Sign is change behavior ( KFIN ( 1 , J ) , J = - 40 , 40 )
func explodeFor(name []node) (out [][]node, ok bool) {
	// have loop
	if len(name) == 0 {
		return
	}
	if len(name) < 8 {
		return
	}
	if name[0].tok != token.LPAREN {
		return
	}
	{
		// replace to copy
		c := append([]node{}, name...)
		name = c
	}
	{
		// compess values
	again:
		for i := 2; i < len(name); i++ {
			if name[i].tok == token.INT &&
				name[i-2].tok != token.IDENT &&
				name[i-2].tok != token.INT {
				switch name[i-1].tok {
				case token.ADD: // +
					// just remove token
					name = append(name[:i-1], name[i:]...)
					goto again
				case token.SUB: // -
					// inject into value
					name[i].b = append([]byte{'-'}, name[i].b...)
					name = append(name[:i-1], name[i:]...)
					goto again
				}
			}
		}
	}
	last := len(name)
	if name[last-1].tok != token.RPAREN {
		return
	}
	var Iend int
	if val, err := strconv.Atoi(string(name[last-2].b)); err != nil {
		return
	} else {
		Iend = val
	}
	var Istart int
	if val, err := strconv.Atoi(string(name[last-4].b)); err != nil {
		return
	} else {
		Istart = val
	}
	var Iname []byte
	Iname = name[last-6].b

	found := false
	for index := 1; index < last-7; index++ {
		if bytes.Equal(name[index].b, Iname) {
			found = true
		}
	}
	if !found {
		return
	}

	// generate names
	for i := Istart; i <= Iend; i++ {
		c := append([]node{}, name...)
		c = c[1 : last-7]
		for par := 0; par < len(c); par++ {
			if bytes.Equal(c[par].b, Iname) {
				c[par].b = []byte(strconv.Itoa(i))
				c[par].tok = token.INT
			}
		}
		// inject names
		out = append(out, c)
	}

	// recursive check again
	var summ [][]node
	for i := range out {
		if e, ok := explodeFor(out[i]); ok {
			summ = append(summ, e...)
			continue
		}
		summ = append(summ, out[i])
	}
	return summ, true
}

// Example:
// DATA GAM , GAMSQ , RGAMSQ / 4096.D0 , 16777216.D0 , 5.9604645D-8 /
//
// LOGICAL            ZSWAP( 4 )
// DATA               ZSWAP / .FALSE., .FALSE., .TRUE., .TRUE. /
//
// INTEGER            IPIVOT( 4, 4 )
// DATA               IPIVOT / 1, 2, 3, 4, 2, 1, 4, 3, 3, 4, 1, 2, 4, 3, 2, 1 /
//
// INTEGER            LOCL12( 4 ), LOCU21( 4 ),
// DATA               LOCU12 / 3, 4, 1, 2 / , LOCL21 / 2, 1, 4, 3 /
//
// DATA (M(I)  ,I= 1, 180) / ... /
// DATA (P(1,I),I= 1, 500) / ... /
//
// TODO:
//
// INTEGER            LV, IPW2
// PARAMETER          ( LV = 128 )
// INTEGER            J
// INTEGER            MM( LV, 4 )
//
// DATA (P(I,4),I= 1, 500) / ... /
// DATA ((D(I,J),J=1,4),I=1,4) / ... /

func (p *parser) parseData() (stmts []goast.Stmt) {
	p.expect(ftData)
	p.ident++

	// parse names and values
	var names [][]node
	names = append(names, []node{})
	var values [][]node
	counter := 0
	isNames := true
	for ; p.ident < len(p.ns); p.ident++ {
		if p.ns[p.ident].tok == ftNewLine {
			break
		}
		if p.ns[p.ident].tok == token.COMMENT {
			continue
		}
		if p.ns[p.ident].tok == token.QUO {
			if isNames {
				values = append(values, []node{})
			}
			isNames = !isNames
			continue
		}
		if p.ns[p.ident].tok == token.LPAREN {
			counter++
		}
		if p.ns[p.ident].tok == token.RPAREN {
			counter--
		}
		if p.ns[p.ident].tok == token.COMMA && counter == 0 {
			if isNames {
				names = append(names, []node{})
			} else {
				values = append(values, []node{})
			}
			continue
		}
		if isNames {
			names[len(names)-1] = append(names[len(names)-1], p.ns[p.ident])
		} else {
			values[len(values)-1] = append(values[len(values)-1], p.ns[p.ident])
		}
	}

	// Explode loops
	{
		var exp [][]node
		for i := range names {
			e, ok := explodeFor(names[i])
			if ok {
				exp = append(exp, e...)
				continue
			}
			exp = append(exp, names[i])
		}
		names = exp
	}

	// Example of names:
	// LL                       - value
	// LL                       - vector fully
	// LL                       - matrix fully
	// LL (1)                   - one value of vector
	// LL (1,1)                 - one value of matrix
	// (LL( J ), J = 1, 4 )     - one row of vector
	// (LL( 1, J ), J = 1, 4 )  - one row of matrix
	type tExpr struct {
		expr   goast.Expr
		isByte bool
	}
	var nameExpr []tExpr
	for _, name := range names {
		if len(name) == 1 {
			// LL                       - value
			// LL                       - vector fully
			// LL                       - matrix fully
			v, ok := p.initVars.get(nodesToString(name))
			if !ok {
				p.initVars.add(nodesToString(name), goType{
					baseType: "float64",
				})
				v, _ = p.initVars.get(nodesToString(name))
			}
			lenArray := p.getArrayLen(v.name)
			isByte := v.typ.getBaseType() == "byte"
			switch lenArray {
			case 0:
				nameExpr = append(nameExpr, tExpr{
					expr:   p.parseExprNodes(name),
					isByte: isByte,
				})
			case 1: // vector
				size, ok := p.getSize(v.name, 0)
				if !ok {
					panic("Not ok : " + v.name)
				}
				for i := 0; i < size; i++ {
					nameExpr = append(nameExpr, tExpr{
						expr: &goast.IndexExpr{
							X:      &goast.StarExpr{X: goast.NewIdent(nodesToString(name))},
							Lbrack: 1,
							Index: &goast.BasicLit{
								Kind:  token.INT,
								Value: strconv.Itoa(i),
							},
						},
						isByte: isByte})
				}
			case 2: // matrix
				size0, _ := p.getSize(v.name, 0)
				size1, _ := p.getSize(v.name, 1)
				for i := 0; i < size0; i++ {
					for j := 0; j < size1; j++ {
						nameExpr = append(nameExpr, tExpr{
							expr: &goast.IndexExpr{
								X: &goast.IndexExpr{
									X:      &goast.StarExpr{X: goast.NewIdent(nodesToString(name))},
									Lbrack: 1,
									Index: &goast.BasicLit{
										Kind:  token.INT,
										Value: strconv.Itoa(j),
									},
								},
								Lbrack: 1,
								Index: &goast.BasicLit{
									Kind:  token.INT,
									Value: strconv.Itoa(i),
								},
							},
							isByte: isByte})
					}
				}
			case 3: //matrix ()()()
				size0, _ := p.getSize(v.name, 0)
				size1, _ := p.getSize(v.name, 1)
				size2, _ := p.getSize(v.name, 2)
				for k := 0; k < size2; k++ {
					for j := 0; j < size1; j++ {
						for i := 0; i < size0; i++ {
							nameExpr = append(nameExpr, tExpr{
								expr: &goast.IndexExpr{
									X: &goast.IndexExpr{
										X: &goast.IndexExpr{
											X:      &goast.StarExpr{X: goast.NewIdent(nodesToString(name))},
											Lbrack: 1,
											Index: &goast.BasicLit{
												Kind:  token.INT,
												Value: strconv.Itoa(i),
											},
										},
										Lbrack: 1,
										Index: &goast.BasicLit{
											Kind:  token.INT,
											Value: strconv.Itoa(j),
										},
									},
									Lbrack: 1,
									Index: &goast.BasicLit{
										Kind:  token.INT,
										Value: strconv.Itoa(k),
									},
								},
								isByte: isByte})
						}
					}
				}
			default:
				panic("Not acceptable type 1 : " + nodesToString(name))
			}

			continue
		}

		if v, ok := p.initVars.get(string(name[0].b)); ok {
			isByte := v.typ.getBaseType() == "byte"

			str := nodesToString(name)
			na := []byte(str)

			nodes := p.parseExprNodes(scan(na))

			nameExpr = append(nameExpr, tExpr{
				expr:   nodes,
				isByte: isByte,
			})

			continue
		}

		panic("Not acceptable type 3 : " + nodesToString(name))
	}

mul:
	for k := range values {
		// Example :
		// DATA R / 5*6 /
		// Equal:
		// DATA R / 6,6,6,6,6 /
		var haveStar bool
		var starPos int
		for i, vi := range values[k] {
			if vi.tok == token.MUL {
				haveStar = true
				starPos = i
				break
			}
		}

		if !haveStar {
			continue
		}

		amount, _ := strconv.Atoi(string(values[k][starPos-1].b))
		var inject [][]node
		for i := 0; i < amount; i++ {
			inject = append(inject, values[k][starPos+1:])
		}
		values = append(values[:k], append(inject, values[k+1:]...)...)
		goto mul
	}

	if len(nameExpr) != len(values) {
		var str string
		for i := range names {
			str += fmt.Sprintln(">>", nodesToString(names[i]))
			v, ok := p.initVars.get(nodesToString(names[i]))
			if ok {
				str += fmt.Sprintln("1) ", v.name)
				str += fmt.Sprintln("2) ", v.typ)
				str += fmt.Sprintln("3) ", v.typ.baseType)
				str += fmt.Sprintln("4) ", v.typ.getBaseType())
				str += fmt.Sprintln("5) ", v.typ.arrayNode)
			}
			if i > 10 {
				str += fmt.Sprintf("and other %d names ...", len(names)-i)
				break
			}
		}
		str += fmt.Sprintln(" amount nameExpr is ", len(nameExpr))
		for i := range nameExpr {
			str += fmt.Sprintln("nameExpr[", i, "] =", nameExpr[i])
			if i > 10 {
				str += fmt.Sprintf("and other %d values ...", len(nameExpr)-i)
				break
			}
		}
		str += fmt.Sprintln(" amount values is ", len(values))
		for i := range values {
			str += fmt.Sprintln("value[", i, "]=", nodesToString(values[i]))
			if i > 10 {
				str += fmt.Sprintf("and other %d values ...", len(values)-i)
				break
			}
		}
		panic(fmt.Errorf("Size is not same %d!=%d\n%v",
			len(nameExpr), len(values), str))
	}

	if len(nameExpr) > 0 {
		var assign goast.AssignStmt
		assign.Tok = token.ASSIGN // =

		for i := range nameExpr {
			if nameExpr[i].isByte {
				e := p.parseExprNodes(values[i])
				e.(*goast.BasicLit).Kind = token.CHAR
				e.(*goast.BasicLit).Value = fmt.Sprintf("'%c'", e.(*goast.BasicLit).Value[1])
				assign.Lhs = append(assign.Lhs, nameExpr[i].expr)
				assign.Rhs = append(assign.Rhs, e)
				continue
			}
			assign.Lhs = append(assign.Lhs, nameExpr[i].expr)
			assign.Rhs = append(assign.Rhs, p.parseExprNodes(values[i]))
		}

		stmts = append(stmts, &assign)
	}

	return
}

// Examples:
//  GO TO 30
//  GO TO ( 40, 80 )IEXC
//  GO TO next,(30, 50, 70, 90, 110)
func (p *parser) parseGoto() (stmts []goast.Stmt) {
	p.expect(token.GOTO)

	p.ident++
	if p.ns[p.ident].tok != token.LPAREN {
		//  GO TO 30
		p.foundLabels["Label"+string(p.ns[p.ident].b)] = true
		stmts = append(stmts, &goast.BranchStmt{
			Tok:   token.GOTO,
			Label: goast.NewIdent("Label" + string(p.ns[p.ident].b)),
		})
		p.ident++
		return
	}
	//  GO TO ( 40, 80, 100 )IEXC
	//  GO TO ( 40 )IEXC

	// parse labels
	p.expect(token.LPAREN)
	var labelNames []string
	for ; p.ident < len(p.ns); p.ident++ {
		var out bool
		switch p.ns[p.ident].tok {
		case token.LPAREN:
			// do nothing
		case token.RPAREN:
			out = true
		case token.COMMA:
			// do nothing
		default:
			labelNames = append(labelNames, string(p.ns[p.ident].b))
			p.foundLabels["Label"+string(p.ns[p.ident].b)] = true
		}
		if out {
			break
		}
	}
	p.expect(token.RPAREN)
	p.ident++

	// ignore COMMA
	if p.ns[p.ident].tok == token.COMMA {
		p.ident++
	}

	if len(labelNames) == 0 {
		panic("Not acceptable amount of labels in GOTO")
	}

	// get expr
	st := p.ident
	for ; p.ident < len(p.ns) && p.ns[p.ident].tok != ftNewLine; p.ident++ {
	}
	// generate Go code
	var sw goast.SwitchStmt
	sw.Tag = p.parseExpr(st, p.ident)
	sw.Body = &goast.BlockStmt{}
	for i := 0; i < len(labelNames); i++ {
		sw.Body.List = append(sw.Body.List, &goast.CaseClause{
			List: []goast.Expr{goast.NewIdent(strconv.Itoa(i + 1))},
			Body: []goast.Stmt{&goast.BranchStmt{
				Tok:   token.GOTO,
				Label: goast.NewIdent("Label" + labelNames[i]),
			}},
		})
	}

	stmts = append(stmts, &sw)

	return
}

//  PARAMETER ( ONE = ( 1.0E+0 , 0.0E+0 )  , ZERO = 0.0E+0 )
//  PARAMETER ( LV = 2 )
func (p *parser) parseParameter() (stmts []goast.Stmt) {
	p.expect(ftParameter)
	p.ident++
	p.expect(token.LPAREN)
	// parse values
	var names [][]node
	counter := 1
	p.ident++
	for ; p.ident < len(p.ns); p.ident++ {
		if p.ns[p.ident].tok == token.LPAREN {
			counter++
		}
		if p.ns[p.ident].tok == token.RPAREN {
			counter--
		}
		if p.ns[p.ident].tok == token.RPAREN && counter == 0 {
			break
		}
		if p.ns[p.ident].tok == token.COMMA && counter == 1 {
			names = append(names, []node{})
			continue
		}
		if len(names) == 0 {
			names = append(names, []node{})
		}
		names[len(names)-1] = append(names[len(names)-1], p.ns[p.ident])
	}

	// split to name and value
	for _, val := range names {
		for i := 0; i < len(val); i++ {
			if val[i].tok == token.ASSIGN {
				// add parameters in parser
				p.constants[nodesToString(val[:i])] = val[i+1:]
			}
		}
	}

	p.expect(token.RPAREN)
	p.ident++
	return
}

func (p *parser) parseAssign() (stmts []goast.Stmt) {
	p.expect(ftAssign)
	p.ident++

	statement := string(p.ns[p.ident].b)
	p.ident++

	// ignore TO
	p.ident++

	intVar := string(p.ns[p.ident].b)
	p.ident++
	stmts = append(stmts, &goast.ExprStmt{
		X: goast.NewIdent("// ASSIGN " + statement + " TO " + intVar),
	}, &goast.AssignStmt{
		Lhs: []goast.Expr{goast.NewIdent(intVar)},
		Tok: token.ASSIGN,
		Rhs: []goast.Expr{goast.NewIdent(statement)},
	}, &goast.AssignStmt{
		Lhs: []goast.Expr{goast.NewIdent("_")},
		Tok: token.ASSIGN,
		Rhs: []goast.Expr{goast.NewIdent(intVar)},
	})

	return
}

// Examples:
//    COMMON/PDAT/LOC(3), T(1)
// Implementation:
// var COMMON MEMORY
// type MEMORY struct {
//		PDAT struct {
//			LOC [3]int
//			T   [1]float64
//		}
// }
//
//	COMMON A,B
//
// type MEMORY struct {
//		A [2]int
//		B [1]float64
// }
//
func (p *parser) parseCommon() (stmts []goast.Stmt) {
	p.expect(ftCommon)
	p.ident++

	var blockName string = "ALL"
	if p.ns[p.ident].tok == token.QUO { // /
		// find block name
		p.ident++
		blockName = string(p.ns[p.ident].b)
		p.ident++

		p.expect(token.QUO) // /
		p.ident++
	}

	// variable names without type
	var names []string
	newIdent := true
	isopen := false
	for ; ; p.ident++ {
		exit := false
		switch v := p.ns[p.ident]; v.tok {
		case token.COMMA:
			if isopen == false {
				newIdent = true
			} else {
				names[len(names)-1] += string(v.b)
			}
			continue
		case ftNewLine:
			exit = true
		default:
			if v.tok == token.LPAREN {
				isopen = true
			}
			if v.tok == token.RPAREN {
				isopen = false
			}
			if newIdent {
				newIdent = false
				names = append(names, string(v.b))
			} else {
				names[len(names)-1] += string(v.b)
			}
		}
		if exit {
			break
		}
	}

	p.expect(ftNewLine)

	// put block name in parser
	var variables []varInitialization
	for i := range names {
		name := names[i]
		var addition []node
		if index := strings.Index(names[i], "("); index > 0 {
			addition = scan([]byte(name[index:]))
			name = name[:index]
		}
		var typ goType
		var typNode node
		if i == 0 {
			typNode = node{tok: ftInteger, b: []byte("INTEGER")}
		} else {
			typNode = node{tok: ftReal, b: []byte("REAL")}
		}
		typ = parseType(append([]node{typNode}, addition...))

		if v, ok := p.initVars.get(name); ok {
			typ = v.typ
		}

		variables = append(variables, varInitialization{name: name, typ: typ})
	}
	p.Common.addBlockName(blockName, variables)

	// generate stmts
	// {{ .name }} = COMMON.{{ .blockName }}.{{ name }}
	for i := range names {

		// if variable is not initialized
		name := names[i]
		if index := strings.Index(names[i], "("); index > 0 {
			name = name[:index]
		}
		if _, ok := p.initVars.get(name); !ok {
			// from:
			//    COMMON LOC(3), T(1)
			// to:
			//    INTEGER LOC(3)
			//    REAL T(1)
			//    COMMON LOC(3), T(1)
			var inject []node
			inject = append(inject, node{tok: ftNewLine, b: []byte("\n")})
			if i == 0 {
				inject = append(inject, node{tok: ftInteger, b: []byte("INTEGER")})
			} else {
				inject = append(inject, node{tok: ftReal, b: []byte("REAL")})
			}
			n := scan([]byte(names[i]))
			inject = append(inject, n...)
			inject = append(inject, node{tok: ftNewLine, b: []byte("\n")})

			if strings.Contains(names[i], "(") {
				inject = append(inject, node{tok: ftNewLine, b: []byte("\n")})
				if i == 0 {
					inject = append(inject, node{tok: ftInteger, b: []byte("INTEGER")})
				} else {
					inject = append(inject, node{tok: ftReal, b: []byte("REAL")})
				}
				n[0].b = append([]byte("COMMON."+blockName+"."), n[0].b...)
				inject = append(inject, n...)
				inject = append(inject, node{tok: ftNewLine, b: []byte("\n")})
			}

			p.ns = append(p.ns[:p.ident], append(inject, p.ns[p.ident:]...)...)
		}

		stmts = append(stmts, &goast.AssignStmt{
			Lhs: []goast.Expr{goast.NewIdent(name)},
			Tok: token.ASSIGN, // =
			Rhs: []goast.Expr{goast.NewIdent("COMMON." + blockName + "." + name)},
		})
	}

	return
}
