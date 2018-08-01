package fortran

import (
	"bytes"
	"fmt"
	goast "go/ast"
	"go/token"
	"strconv"
	"strings"
)

type parser struct {
	// sc    *scanner
	ast   goast.File
	ident int
	ns    []ele

	functionExternalName []string
	initVars             []initialVar

	// import packeges
	pkgs map[string]bool

	// label of DO
	endLabelDo map[string]int

	errs []error
}

func (p *parser) addImport(pkg string) {
	p.pkgs[pkg] = true
}

type initialVar struct {
	name string
	typ  string
}

func (p *parser) init() {
	p.functionExternalName = make([]string, 0)
	p.pkgs = map[string]bool{}
	p.endLabelDo = map[string]int{}
}

func parse(b []byte) (ast goast.File, err []error) {
	var p parser

	l := scanT(b)
	for e := l.Front(); e != nil; e = e.Next() {
		p.ns = append(p.ns, *e.Value.(*ele))
	}

	p.ast.Name = goast.NewIdent("main")

	var decls []goast.Decl
	p.ident = 0
	decls = p.parseNodes()
	if len(p.errs) > 0 {
		return p.ast, p.errs
	}

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

	p.ast.Decls = append(p.ast.Decls, decls...)

	return p.ast, p.errs
}

func (p *parser) parseNodes() (decls []goast.Decl) {

	if p.ident < 0 || p.ident >= len(p.ns) {
		p.errs = append(p.errs,
			fmt.Errorf("Ident is outside nodes: %d/%d", p.ident, len(p.ns)))
		return
	}

	// find all names of FUNCTION, SUBROUTINE, PROGRAM
	var internalFunction []string
	for ; p.ident < len(p.ns); p.ident++ {
		switch p.ns[p.ident].tok {
		case SUBROUTINE:
			p.expect(SUBROUTINE)
			p.ident++
			p.expect(token.IDENT)
			internalFunction = append(internalFunction, string(p.ns[p.ident].b))
			continue
		case PROGRAM:
			p.expect(PROGRAM)
			p.ident++
			p.expect(token.IDENT)
			internalFunction = append(internalFunction, string(p.ns[p.ident].b))
			continue
		}

		// Example:
		//   RECURSIVE SUBROUTINE CGELQT3( M, N, A, LDA, T, LDT, INFO )
		if strings.ToUpper(string(p.ns[p.ident].b)) == "RECURSIVE" {
			p.ns[p.ident].tok, p.ns[p.ident].b = NEW_LINE, []byte("\n")
			continue
		}

		// FUNCTION
		for i := p.ident; i < len(p.ns) && p.ns[i].tok != NEW_LINE; i++ {
			if p.ns[p.ident].tok == FUNCTION {
				p.expect(FUNCTION)
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
		case NEW_LINE:
			next = true // TODO
		case token.COMMENT:
			next = true // TODO
		case SUBROUTINE: // SUBROUTINE
			var decl goast.Decl
			decl = p.parseSubroutine()
			decls = append(decls, decl)
			next = true
		case PROGRAM: // PROGRAM
			var decl goast.Decl
			decl = p.parseProgram()
			decls = append(decls, decl)
			next = true
		default:
			// Example :
			//  COMPLEX FUNCTION CDOTU ( N , CX , INCX , CY , INCY )
			for i := p.ident; i < len(p.ns) && p.ns[i].tok != NEW_LINE; i++ {
				if p.ns[i].tok == FUNCTION {
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
		case NEW_LINE, token.EOF:
			continue
		}

		// if at the begin we haven't SUBROUTINE , FUNCTION,...
		// then add fake Program
		var comb []ele
		comb = append(comb, p.ns[:p.ident]...)
		comb = append(comb, []ele{
			ele{tok: NEW_LINE, b: []byte("\n")},
			ele{tok: PROGRAM, b: []byte("PROGRAM")},
			ele{tok: token.IDENT, b: []byte("MAIN")},
			ele{tok: NEW_LINE, b: []byte("\n")},
		}...)
		comb = append(comb, p.ns[p.ident:]...)
		p.ns = comb
		p.ident--

		p.addError("Add fake PROGRAM MAIN")
	}

	return
}

func (p *parser) gotoEndLine() {
	_ = p.getLine()
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
	for ; p.ident >= 0 && p.ns[p.ident].tok != NEW_LINE; p.ident-- {
	}
	p.ident++
	for ; p.ident < len(p.ns) && p.ns[p.ident].tok != NEW_LINE; p.ident++ {
		line += " " + string(p.ns[p.ident].b)
	}
	return
}

// go/ast Visitor for parse FUNCTION
type vis struct {
	from, to string
}

func (v vis) Visit(node goast.Node) (w goast.Visitor) {
	if ident, ok := node.(*goast.Ident); ok {
		if ident.Name == v.from {
			ident.Name = v.to
		}
	}
	return v
}

// Example :
//  COMPLEX FUNCTION CDOTU ( N , CX , INCX , CY , INCY )
//  DOUBLE PRECISION FUNCTION DNRM2 ( N , X , INCX )
//  COMPLEX * 16 FUNCTION ZDOTC ( N , ZX , INCX , ZY , INCY )
func (p *parser) parseFunction() (decl goast.Decl) {
	var fd goast.FuncDecl
	fd.Type = &goast.FuncType{
		Params: &goast.FieldList{},
	}

	var returnType []ele
	for ; p.ns[p.ident].tok != FUNCTION && p.ns[p.ident].tok != NEW_LINE; p.ident++ {
		returnType = append(returnType, p.ns[p.ident])
	}
	p.expect(FUNCTION)

	p.ident++
	p.expect(token.IDENT)
	name := string(p.ns[p.ident].b)
	fd.Name = goast.NewIdent(name)
	returnName := name + "_RES"
	fd.Type.Results = &goast.FieldList{
		List: []*goast.Field{
			&goast.Field{
				Names: []*goast.Ident{goast.NewIdent(returnName)},
				Type:  goast.NewIdent(p.parseType(returnType)),
			},
		},
	}

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

	// add correct type of subroutine arguments
	p.argumentCorrection(fd)

	// init vars
	fd.Body.List = append(p.initializeVars(), fd.Body.List...)

	// change function name variable to returnName
	v := vis{
		from: name,
		to:   returnName,
	}
	goast.Walk(v, fd.Body)

	decl = &fd
	return
}

// delete external function type definition
func (p *parser) removeExternalFunction() {
checkExternalFunction:
	for i := range p.initVars {
		var remove bool
		for _, f := range p.functionExternalName {
			if p.initVars[i].name == f {
				remove = true
				break
			}
		}
		if remove {
			// fmt.Println("Remove external function definition: ", name)
			p.initVars = append(p.initVars[:i], p.initVars[i+1:]...)
			goto checkExternalFunction
		}
	}
}

// add correct type of subroutine arguments
func (p *parser) argumentCorrection(fd goast.FuncDecl) {
checkArguments:
	for i := range fd.Type.Params.List {
		fieldName := fd.Type.Params.List[i].Names[0].Name
		for j := range p.initVars {
			if fieldName == p.initVars[j].name {
				fd.Type.Params.List[i].Type = goast.NewIdent(p.initVars[j].typ)

				// fmt.Println("Remove to arg : ", fieldName)
				p.initVars = append(p.initVars[:j], p.initVars[j+1:]...)
				goto checkArguments
			}
		}
	}
}

// init vars
func (p *parser) initializeVars() (vars []goast.Stmt) {
	for i := range p.initVars {
		vars = append(vars, &goast.DeclStmt{
			Decl: &goast.GenDecl{
				Tok: token.VAR,
				Specs: []goast.Spec{
					&goast.ValueSpec{
						Names: []*goast.Ident{goast.NewIdent(p.initVars[i].name)},
						Type:  goast.NewIdent(p.initVars[i].typ),
					},
				},
			},
		})
	}
	return
}

func (p *parser) parseProgram() (decl goast.Decl) {
	p.expect(PROGRAM)

	p.ns[p.ident].tok = SUBROUTINE

	return p.parseSubroutine()
}

// Example :
//  SUBROUTINE CHBMV ( UPLO , N , K , ALPHA , A , LDA , X , INCX , BETA , Y , INCY )
func (p *parser) parseSubroutine() (decl goast.Decl) {
	var fd goast.FuncDecl
	fd.Type = &goast.FuncType{
		Params: &goast.FieldList{},
	}

	p.expect(SUBROUTINE)

	p.ident++
	p.expect(token.IDENT)
	name := string(p.ns[p.ident].b)
	fd.Name = goast.NewIdent(name)

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

	// add correct type of subroutine arguments
	p.argumentCorrection(fd)

	// init vars
	fd.Body.List = append(p.initializeVars(), fd.Body.List...)

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
		for _, err := range p.errs {
			fmt.Println("Error : ", err.Error())
		}
		// Panic
		panic(fmt.Errorf("Expect %s, but we have {{%s,%s}}. Pos = %v",
			view(t), view(p.ns[p.ident].tok), string(p.ns[p.ident].b),
			p.ns[p.ident].pos))
	}
}

func (p *parser) parseListStmt() (stmts []goast.Stmt) {
	for p.ident < len(p.ns) {

		if p.ns[p.ident].tok == token.COMMENT {
			// TODO : stmts = append(stmts, &goast.ExprStmt{X: goast.NewIdent(p.ns[p.ident].lit)})
			p.ident++
			continue
		}
		if p.ns[p.ident].tok == NEW_LINE {
			p.ident++
			continue
		}

		if p.ns[p.ident].tok == END {
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
		if stmt == nil {
			// p.addError("stmt is nil in line ")
			// break
			continue
		}
		stmts = append(stmts, stmt...)
	}
	return
}

// Examples:
//  INTEGER
//  COMPLEX
//  COMPLEX*16
//  REAL (LDA,*)
//  DOUBLE PRECISION (*)
//  LOGICAL
//  CHARACTER*32
func (p *parser) parseType(nodes []ele) (typ string) {

	typ = "int"
	switch nodes[0].tok {
	case INTEGER:
		typ = "int"
	case LOGICAL:
		typ = "bool"
	case CHARACTER:
		typ = "byte"
		// From:
		//  CHARACTER * 16
		// To:
		//  CHARACTER (16)

		if len(nodes) > 2 && nodes[1].tok == token.MUL {
			if nodes[2].tok == token.INT {
				nodes = []ele{
					nodes[0],
					ele{
						tok: token.LPAREN,
						b:   []byte("("),
					},
					nodes[2],
					ele{
						tok: token.RPAREN,
						b:   []byte(")"),
					},
				}
			}
		}
	case COMPLEX:
		typ = "complex128"
	case REAL:
		typ = "float64"
	case DOUBLE:
		typ = "float64"
		// next may be PRECISION
		if nodes[1].tok == PRECISION {
			// ignore
			nodes = nodes[1:]
		}
	default:
		p.addError("Cannot parse type format: " + string(nodes[0].b))
		return
	}

	nodes = nodes[1:]
	if len(nodes) == 0 {
		return
	}

	// * 16
	if nodes[0].tok == token.MUL {
		nodes = nodes[1:]
		if nodes[0].tok == token.INT {
			nodes = nodes[1:]
		}
	}
	if len(nodes) == 0 {
		return
	}

	arraySize := 1
	if nodes[0].tok != token.LPAREN {
		p.addError("Cannot parse part of type " + nodesToString(nodes))
		return
	}

	for nodes[0].tok != token.RPAREN {
		if nodes[0].tok == token.COMMA {
			arraySize++
		}
		nodes = nodes[1:]
		if len(nodes) == 0 {
			p.addError("Cannot parse type : not expected end of nodes")
			return
		}
	}

	if nodes[0].tok != token.RPAREN {
		p.addError("Cannot parse part of type " + nodesToString(nodes))
		return
	}
	nodes = nodes[1:]

	for i := 0; i < arraySize; i++ {
		typ = "[]" + typ
	}

	// CHARACTER(1) SRNAME_ARRAY(SRNAME_LEN)
	//                          ============
	// parse
	if len(nodes) == 3 {
		nodes = []ele{}
	}

	if len(nodes) != 0 {
		p.addError("Cannot parse type at the end : " + nodesToString(nodes))
		return
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
	var baseType []ele
	for ; p.ns[p.ident].tok != token.IDENT; p.ident++ {
		baseType = append(baseType, p.ns[p.ident])
	}
	p.expect(token.IDENT)

	var name string
	var additionType []ele
	for ; p.ns[p.ident].tok != NEW_LINE &&
		p.ns[p.ident].tok != token.EOF; p.ident++ {
		// parse name
		p.expect(token.IDENT)
		name = string(p.ns[p.ident].b)

		// parse addition type
		additionType = []ele{}
		p.ident++
		for ; p.ns[p.ident].tok != NEW_LINE &&
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
					case NEW_LINE:
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
		p.initVars = append(p.initVars, initialVar{
			name: name,
			typ:  p.parseType(append(baseType, additionType...)),
		})
		if p.ns[p.ident].tok != token.COMMA {
			p.ident--
		}
	}

	return
}

func (p *parser) parseDoWhile() (sDo goast.ForStmt) {
	p.expect(DO)
	p.ident++
	p.expect(WHILE)
	p.ident++
	start := p.ident
	for ; p.ident < len(p.ns); p.ident++ {
		if p.ns[p.ident].tok == NEW_LINE {
			break
		}
	}
	sDo.Cond = p.parseExpr(start, p.ident)

	p.expect(NEW_LINE)
	p.ident++

	sDo.Body = &goast.BlockStmt{
		Lbrace: 1,
		List:   p.parseListStmt(),
	}

	return
}

func (p *parser) parseDo() (sDo goast.ForStmt) {
	p.expect(DO)
	p.ident++
	if p.ns[p.ident].tok == WHILE {
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
	name := string(p.ns[p.ident].b)

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
			goast.NewIdent(name),
		},
		Tok: token.ASSIGN,
		Rhs: []goast.Expr{
			p.parseExpr(start, p.ident),
		},
	}

	p.expect(token.COMMA)

	// Cond is expression
	p.ident++
	start = p.ident
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
		if (p.ns[p.ident].tok == token.COMMA || p.ns[p.ident].tok == NEW_LINE) &&
			counter == 0 {
			break
		}
	}
	sDo.Cond = &goast.BinaryExpr{
		X:  goast.NewIdent(name),
		Op: token.LSS,
		Y:  p.parseExpr(start, p.ident),
	}

	if p.ns[p.ident].tok == NEW_LINE {
		sDo.Post = &goast.IncDecStmt{
			X:   goast.NewIdent(name),
			Tok: token.INC,
		}
	} else {
		p.expect(token.COMMA)
		p.ident++

		// Post is expression
		start = p.ident
		for ; p.ident < len(p.ns); p.ident++ {
			if p.ns[p.ident].tok == NEW_LINE {
				break
			}
		}
		sDo.Post = &goast.AssignStmt{
			Lhs: []goast.Expr{goast.NewIdent(name)},
			Tok: token.ADD_ASSIGN,
			Rhs: []goast.Expr{p.parseExpr(start, p.ident)},
		}
	}

	p.expect(NEW_LINE)

	sDo.Body = &goast.BlockStmt{
		Lbrace: 1,
		List:   p.parseListStmt(),
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

	sIf.Cond = p.parseExpr(start, p.ident)

	p.expect(token.RPAREN)
	p.ident++

	if p.ns[p.ident].tok == THEN {
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
	p.expect(EXTERNAL)

	p.ident++
	for ; p.ns[p.ident].tok != token.EOF; p.ident++ {
		if p.ns[p.ident].tok == NEW_LINE {
			p.ident++
			break
		}
		switch p.ns[p.ident].tok {
		case token.IDENT:
			name := string(p.ns[p.ident].b)
			p.functionExternalName = append(p.functionExternalName, name)
			// fmt.Println("Function external: ", name)
		case token.COMMA:
			// ingore
		default:
			p.addError("Cannot parse External " + string(p.ns[p.ident].b))
		}
	}
}

func (p *parser) parseStmt() (stmts []goast.Stmt) {
	switch p.ns[p.ident].tok {
	case INTEGER, CHARACTER, COMPLEX, LOGICAL, REAL, DOUBLE:
		stmts = append(stmts, p.parseInit()...)

	case token.RETURN:
		stmts = append(stmts, &goast.ReturnStmt{})
		p.ident++
		p.expect(NEW_LINE)

	case EXTERNAL:
		p.parseExternal()

	case NEW_LINE:
		// ignore
		p.ident++

	case token.IF:
		sIf := p.parseIf()
		stmts = append(stmts, &sIf)

	case DO:
		sDo := p.parseDo()
		stmts = append(stmts, &sDo)

	case CALL:
		// Example:
		// CALL XERBLA ( 'CGEMM ' , INFO )
		p.expect(CALL)
		p.ident++
		start := p.ident
		for ; p.ns[p.ident].tok != NEW_LINE; p.ident++ {
		}
		stmts = append(stmts, &goast.ExprStmt{
			X: p.parseExpr(start, p.ident),
		})
		p.expect(NEW_LINE)

	case INTRINSIC:
		// Example:
		//  INTRINSIC CONJG , MAX
		p.expect(INTRINSIC)
		p.ident++
		for ; p.ident < len(p.ns) && p.ns[p.ident].tok != NEW_LINE; p.ident++ {
			switch p.ns[p.ident].tok {
			case token.IDENT:
				p.functionExternalName = append(p.functionExternalName,
					string(p.ns[p.ident].b))
			case token.COMMA:
				// ignore
			case INTEGER, CHARACTER, COMPLEX, LOGICAL, REAL:
				// type conversion - ignore
			default:
				p.addError("Cannot parse function name in INTRINSIC:" +
					string(p.ns[p.ident].b))
			}
		}
		p.expect(NEW_LINE)

	case DATA:
		// Example:
		// DATA GAM , GAMSQ , RGAMSQ / 4096.D0 , 16777216.D0 , 5.9604645D-8 /
		sData := p.parseData()
		stmts = append(stmts, sData...)

	case WRITE:
		sWrite := p.parseWrite()
		stmts = append(stmts, sWrite...)

	case STOP:
		p.expect(STOP)
		p.ident++
		p.expect(NEW_LINE)
		stmts = append(stmts, &goast.ReturnStmt{})

	case token.GOTO:
		// Examples:
		//  GO TO 30
		//  GO TO ( 40, 80 )IEXC
		sGoto := p.parseGoto()
		stmts = append(stmts, sGoto...)
		p.expect(NEW_LINE)

	case IMPLICIT:
		// TODO: add support IMPLICIT
		var nodes []ele
		for ; p.ident < len(p.ns); p.ident++ {
			if p.ns[p.ident].tok == NEW_LINE || p.ns[p.ident].tok == token.EOF {
				break
			}
			nodes = append(nodes, p.ns[p.ident])
		}
		// p.addError("IMPLICIT is not support.\n" + nodesToString(nodes))
		// ignore
		_ = nodes

	case token.INT:
		if p.ns[p.ident+1].tok == token.CONTINUE {
			label := string(p.ns[p.ident].b)
			stmts = append(stmts, &goast.LabeledStmt{
				Label: goast.NewIdent("Label" + label),
				Colon: 1,
				Stmt:  &goast.EmptyStmt{},
			})
			// replace CONTINUE to NEW_LINE
			p.ident++
			p.ns[p.ident].tok, p.ns[p.ident].b = NEW_LINE, []byte("\n")

			// if label have END DO, then add them
			if v, ok := p.endLabelDo[label]; ok && v > 0 {
				var add []ele
				for j := 0; j < v; j++ {
					add = append(add, []ele{
						ele{tok: NEW_LINE, b: []byte("\n")},
						ele{tok: END, b: []byte("END")},
						ele{tok: NEW_LINE, b: []byte("\n")},
					}...)
				}
				var comb []ele
				comb = append(comb, p.ns[:p.ident]...)
				comb = append(comb, add...)
				comb = append(comb, p.ns[p.ident:]...)
				p.ns = comb
			}

			return
		}

		// TODO: add support INT
		var nodes []ele
		for ; p.ident < len(p.ns); p.ident++ {
			if p.ns[p.ident].tok == NEW_LINE || p.ns[p.ident].tok == token.EOF {
				break
			}
			nodes = append(nodes, p.ns[p.ident])
		}
		p.addError("INT is not support.\n" + nodesToString(nodes))

	default:
		start := p.ident
		for ; p.ident < len(p.ns); p.ident++ {
			if p.ns[p.ident].tok == NEW_LINE {
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
			stmts = append(stmts, &goast.AssignStmt{
				Lhs: []goast.Expr{p.parseExpr(start, pos)},
				Tok: token.ASSIGN,
				Rhs: []goast.Expr{p.parseExpr(pos+1, p.ident)},
			})
		} else {
			stmts = append(stmts, &goast.ExprStmt{
				X: p.parseExpr(start, p.ident),
			})
		}

		p.ident++
	}

	return
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
			id := string(p.ns[p.ident].b)
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
	p.expect(NEW_LINE)

	return
}

// Example:
// DATA GAM , GAMSQ , RGAMSQ / 4096.D0 , 16777216.D0 , 5.9604645D-8 /
//
// TODO :
//
// LOGICAL            ZSWAP( 4 )
// DATA               ZSWAP / .FALSE., .FALSE., .TRUE., .TRUE. /
//
// INTEGER            IPIVOT( 4, 4 )
// DATA               IPIVOT / 1, 2, 3, 4, 2, 1, 4, 3, 3, 4, 1, 2, 4,
//      $                   3, 2, 1 /
//
func (p *parser) parseData() (stmts []goast.Stmt) {
	p.expect(DATA)
	p.ident++

	type dis struct {
		start, end int
	}
	var (
		names  []string
		values []dis
	)
	// find names
	for ; p.ident < len(p.ns); p.ident++ {
		var exit bool
		switch p.ns[p.ident].tok {
		case token.IDENT:
			names = append(names, string(p.ns[p.ident].b))
		case token.COMMA:
			// ignore
		case token.QUO: // /
			exit = true
		default:
			p.addError("Cannot parse name in Data :" + string(p.ns[p.ident].b))
		}
		if exit {
			break
		}
	}
	// find values
	p.expect(token.QUO)
	p.ident++
	valPos := 0
	for ; p.ident < len(p.ns); p.ident++ {
		var exit bool
		switch p.ns[p.ident].tok {
		case token.INT, token.FLOAT, token.STRING:
			values = append(values, dis{
				start: p.ident,
				end:   p.ident + 1,
			})
		case token.COMMA:
			// ignore
		case token.QUO: // /
			exit = true
		default:
			p.addError("Cannot parse value in Data :" + string(p.ns[p.ident].b))
		}
		if exit {
			break
		}
		valPos++
	}
	p.expect(token.QUO)
	p.ident++

	// create stmts
	if len(names) != len(values) {
		p.addError("Cannot create stmts in DATA: " +
			" names " + fmt.Sprintf("%d", len(names)) +
			" values " + fmt.Sprintf("%d", len(values)))
		return
	}

	for i := range names {
		stmts = append(stmts, &goast.AssignStmt{
			Lhs: []goast.Expr{goast.NewIdent(names[i])},
			Tok: token.ASSIGN,
			Rhs: []goast.Expr{p.parseExpr(values[i].start, values[i].end)},
		})
	}
	return
}

// Examples:
//  GO TO 30
//  GO TO ( 40, 80 )IEXC
func (p *parser) parseGoto() (stmts []goast.Stmt) {
	p.expect(token.GOTO)

	p.ident++
	if p.ns[p.ident].tok != token.LPAREN {
		//  GO TO 30
		stmts = append(stmts, &goast.BranchStmt{
			Tok:   token.GOTO,
			Label: goast.NewIdent("Label" + string(p.ns[p.ident].b)),
		})
		p.ident++
		return
	}
	// From:
	//  GO TO ( 40, 80, 100 )IEXC
	// To:
	// if IEXC == 2 {
	// 	goto Label80
	// } else if IEXC == 3 {
	// 	goto Label100
	// } else {
	// 	goto Label40
	// }
	//
	// From:
	//  GO TO ( 40 )IEXC
	// To:
	//  goto Label40

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
		}
		if out {
			break
		}
	}

	if len(labelNames) == 0 {
		panic("Not acceptable amount of labels in GOTO")
	}

	// if only one label
	if len(labelNames) == 1 {
		stmts = append(stmts, &goast.BranchStmt{
			Tok:   token.GOTO,
			Label: goast.NewIdent("Label" + string(p.ns[p.ident].b)),
		})
		p.gotoEndLine()
		return
	}

	// if many labels

	// get expr
	p.ident++
	st := p.ident
	for ; p.ident < len(p.ns) && p.ns[p.ident].tok != NEW_LINE; p.ident++ {
	}
	// generate Go code
	var sw goast.SwitchStmt
	sw.Tag = p.parseExpr(st, p.ident)
	sw.Body = &goast.BlockStmt{}
	for i := 1; i < len(labelNames); i++ {
		sw.Body.List = append(sw.Body.List, &goast.CaseClause{
			List: []goast.Expr{goast.NewIdent(strconv.Itoa(i + 1))},
			Body: []goast.Stmt{&goast.BranchStmt{
				Tok:   token.GOTO,
				Label: goast.NewIdent("Label" + labelNames[i]),
			}},
		})
	}
	sw.Body.List = append(sw.Body.List, &goast.CaseClause{
		Body: []goast.Stmt{&goast.BranchStmt{
			Tok:   token.GOTO,
			Label: goast.NewIdent("Label" + labelNames[0]),
		}},
	})

	stmts = append(stmts, &sw)

	return
}

// Example:
//  WRITE ( * , FMT = 9999 ) SRNAME ( 1 : LEN_TRIM ( SRNAME ) ) , INFO
//  9999 FORMAT ( ' ** On entry to ' , A , ' parameter number ' , I2 , ' had ' , 'an illegal value' )
func (p *parser) parseWrite() (stmts []goast.Stmt) {
	p.expect(WRITE)
	p.ident++
	p.expect(token.LPAREN)
	p.ident++
	p.expect(token.MUL)
	p.ident++
	p.expect(token.COMMA)
	p.ident++

	if p.ns[p.ident].tok == token.IDENT && bytes.Equal(p.ns[p.ident].b, []byte("FMT")) {
		p.ident++
		p.expect(token.ASSIGN)
		p.ident++
		p.expect(token.INT)
		fs := p.parseFormat(p.getLineByLabel(p.ns[p.ident].b)[2:])
		p.addImport("fmt")
		p.ident++
		p.expect(token.RPAREN)
		p.ident++
		// separate to expression by comma
		var exprs []goast.Expr
		st := p.ident
		for ; p.ns[p.ident].tok != NEW_LINE; p.ident++ {
			for ; p.ns[p.ident].tok != token.COMMA && p.ns[p.ident].tok != NEW_LINE; p.ident++ {
			}
			// parse expr
			exprs = append(exprs, p.parseExpr(st, p.ident))
			st = p.ident + 1
			if p.ns[p.ident].tok == NEW_LINE {
				p.ident--
			}
		}
		p.expect(NEW_LINE)
		var args []goast.Expr
		args = append(args, goast.NewIdent(fs))
		args = append(args, exprs...)
		stmts = append(stmts, &goast.ExprStmt{
			X: &goast.CallExpr{
				Fun: &goast.SelectorExpr{
					X:   goast.NewIdent("fmt"),
					Sel: goast.NewIdent("Printf"),
				},
				Lparen: 1,
				Args:   args,
			},
		})
	} else {
		panic(fmt.Errorf("Not support in WRITE : %v", string(p.ns[p.ident].b)))
	}

	return
}

func (p *parser) getLineByLabel(label []byte) (fs []ele) {
	var found bool
	var st int
	for st = p.ident; st < len(p.ns); st++ {
		if p.ns[st-1].tok == NEW_LINE && bytes.Equal(p.ns[st].b, label) {
			found = true
			break
		}
	}
	if !found {
		p.addError("Cannot found label :" + string(label))
		return
	}

	for i := st; i < len(p.ns) && p.ns[i].tok != NEW_LINE; i++ {
		fs = append(fs, p.ns[i])
		// remove line
		p.ns[i].tok, p.ns[i].b = NEW_LINE, []byte("\n")
	}

	return
}

func (p *parser) parseFormat(fs []ele) (s string) {
	for _, f := range fs {
		switch f.tok {
		case token.STRING:
			str := string(f.b)
			str = strings.Replace(str, "'", "", -1)
			s += str
		case token.COMMA, token.LPAREN, token.RPAREN:
			// ignore
		default:
			s += "%v"
		}
	}
	return "\"" + s + "\""
}
