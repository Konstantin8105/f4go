package fortran

import (
	"fmt"
	goast "go/ast"
	"go/token"
	"log"
	"os"
)

type parser struct {
	// sc    *scanner
	ast   goast.File
	ident int
	ns    []node

	functionExternalName []string
	initVars             []initialVar

	// import packeges
	pkgs map[string]bool

	// logger
	logger *log.Logger

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
}

//
// func (p *parser) prepare() {
//
// 	var last token.Token
// 	_ = last
// 	for {
// 		tok, lit := p.sc.scan()
// 		if tok == token.EOF {
// 			break
// 		}
//
// 		if tok == token.COMMENT {
// 			continue
// 		}
//
// 		// From:
// 		//  END SUBROUTINE
// 		//  END IF
// 		// To:
// 		//  END
// 		// if last == END && tok != NEW_LINE {
// 		// 	for tok != token.EOF && tok != NEW_LINE && tok != token.ILLEGAL {
// 		// 		tok, lit = p.sc.scan()
// 		// 	}
// 		// 	p.ns = append(p.ns, node{
// 		// 		tok: tok,
// 		// 		lit: lit,
// 		// 	})
// 		// 	last = tok
// 		// 	continue
// 		// }
// 		//
// 		// if last == NEW_LINE && tok == NEW_LINE {
// 		// 	continue
// 		// }
//
// 		// Multiline function arguments
// 		// From:
// 		//  9999 FORMAT ( ' ** On entry to ' , A , ' parameter number ' , I2 , ' had ' ,
// 		//  'an illegal value' )
// 		// To:
// 		//  9999 FORMAT ( ' ** On entry to ' , A , ' parameter number ' , I2 , ' had ' , 'an illegal value' )
// 		// if last == token.COMMA && tok == NEW_LINE {
// 		// 	continue
// 		// }
//
// 		p.ns = append(p.ns, node{
// 			tok: tok,
// 			lit: lit,
// 		})
// 		last = tok
// 	}
//
// 	if len(p.ns) > 0 && p.ns[0].tok == NEW_LINE {
// 		p.ns = p.ns[1:]
// 	}
//
// 	// Simplification DO
// 	//-------------
// 	// From:
// 	//  DO 40 J = 1 , N
// 	//  DO 30 I = 1 , M
// 	//  C ( I , J ) = BETA * C ( I , J )
// 	//  30 CONTINUE
// 	//  40 CONTINUE
// 	//
// 	// Or from:
// 	//  DO 30 J = 1 , N
// 	//  DO 30 I = 1 , M
// 	//  C ( I , J ) = BETA * C ( I , J )
// 	//  30 CONTINUE
// 	//
// 	//-------------
// 	// To:
// 	//  DO J = 1 , N
// 	//  DO I = 1 , M
// 	//  C ( I , J ) = BETA * C ( I , J )
// 	//  END
// 	//  END
// 	//-------------
// 	// 	doLabels := map[string]int{}
// 	// doLabelAgain:
// 	// 	for i := range p.ns {
// 	// 		if i == 0 {
// 	// 			continue
// 	// 		}
// 	// 		if p.ns[i-1].tok == DO && p.ns[i].tok == token.INT {
// 	// 			doLabels[p.ns[i].lit]++
// 	// 			p.ns = append(p.ns[:i], p.ns[i+1:]...)
// 	// 			goto doLabelAgain
// 	// 		}
// 	// 	}
// 	// again:
// 	// 	for i := range p.ns {
// 	// 		if i == 0 {
// 	// 			continue
// 	// 		}
// 	// 		if p.ns[i-1].tok == token.INT && p.ns[i].tok == token.CONTINUE {
// 	// 			if v, ok := doLabels[p.ns[i-1].lit]; ok {
// 	// 				if v <= 0 {
// 	// 					panic("Not acceptable")
// 	// 				}
// 	// 				p.ns[i-1].tok, p.ns[i-1].lit = END, "end"
// 	// 				p.ns[i].tok, p.ns[i].lit = NEW_LINE, "\n"
// 	//
// 	// 				var inject []node
// 	// 				for j := 1; j < v; j++ {
// 	// 					inject = append(inject, []node{
// 	// 						node{
// 	// 							tok: END,
// 	// 							lit: "end",
// 	// 						},
// 	// 						node{
// 	// 							tok: NEW_LINE,
// 	// 							lit: "\n",
// 	// 						},
// 	// 					}...)
// 	// 				}
// 	//
// 	// 				if len(inject) > 0 {
// 	// 					inject = append(inject, p.ns[i+1:]...)
// 	// 					p.ns = append(p.ns[:i+1], inject...)
// 	// 					goto again
// 	// 				}
// 	// 			}
// 	// 		}
// 	// 	}
//
// 	// Simplification of PARAMETER:
// 	// From:
// 	//  PARAMETER ( ONE = ( 1.0E+0 , 0.0E+0 )  , ZERO = 0.0E+0 )
// 	// To:
// 	//  ONE = ( 1.0E+0 , 0.0E+0 )
// 	//  ZERO = 0.0E+0
// 	//
// 	for i := 0; i < len(p.ns); i++ {
// 		if p.ns[i].tok != PARAMETER {
// 			continue
// 		}
// 		// replace PARAMETER to NEW_LINE
// 		p.ns[i].tok, p.ns[i].lit = NEW_LINE, "\n"
// 		i++
// 		// replace ( to NEW_LINE
// 		if p.ns[i].tok != token.LPAREN {
// 			panic("is not LPAREN")
// 		}
// 		p.ns[i].tok, p.ns[i].lit = NEW_LINE, "\n"
// 		i++
// 		// find end )
// 		counter := 1
// 		for j := i; p.ns[j].tok != NEW_LINE; j, i = j+1, i+1 {
// 			if p.ns[j].tok == token.LPAREN {
// 				counter++
// 			}
// 			if p.ns[j].tok == token.RPAREN {
// 				counter--
// 			}
// 			if counter == 1 && p.ns[j].tok == token.COMMA {
// 				// replace , to NEW_LINE
// 				p.ns[i].tok, p.ns[i].lit = NEW_LINE, "\n"
// 			}
// 			if counter == 0 {
// 				break
// 			}
// 		}
// 		// replace ) to NEW_LINE
// 		if p.ns[i].tok != token.RPAREN {
// 			panic("is not RPAREN : " + view(p.ns[i].tok))
// 		}
// 		p.ns[i].tok, p.ns[i].lit = NEW_LINE, "\n"
// 		i++
// 	}
//
// 	// replace string concatenation
// 	// for i := range p.ns {
// 	// 	if p.ns[i].tok == STRING_CONCAT {
// 	// 		p.ns[i] = node{tok: token.ADD, lit: "+"}
// 	// 	}
// 	// }
//
// 	// Multiline expression
// 	// From:
// 	//  IF ( ( M .EQ. 0 ) .OR. ( N .EQ. 0 ) .OR.
// 	//  + ( ( ( ALPHA .EQ. ZERO ) .OR. ( K .EQ. 0 ) ) .AND. ( BETA .EQ. ONE ) ) ) RETURN
// 	// To:
// 	//  IF ( ( M .EQ. 0 ) .OR. ( N .EQ. 0 ) .OR. ( ( ( ALPHA .EQ. ZERO ) .OR. ( K .EQ. 0 ) ) .AND. ( BETA .EQ. ONE ) ) ) RETURN
// 	//
// 	// From:
// 	//  NORM = SCALE * DSQRT ( ( CDABS ( CA / DCMPLX ( SCALE , 0.0d0 ) ) ) ** 2 +
// 	//  ( CDABS ( CB / DCMPLX ( SCALE , 0.0d0 ) ) ) ** 2 )
// 	// To:
// 	//  NORM = SCALE * DSQRT ( ( CDABS ( CA / DCMPLX ( SCALE , 0.0d0 ) ) ) ** 2 + ( CDABS ( CB / DCMPLX ( SCALE , 0.0d0 ) ) ) ** 2 )
// 	isOp := func(t token.Token) bool {
// 		switch t {
// 		case token.ADD, // +
// 			token.SUB, // -
// 			token.MUL, // *
// 			// token.QUO, // /
// 			// token.REM, // %
//
// 			token.AND, // &
// 			token.OR,  // |
// 			token.XOR, // ^
// 			token.SHL, // <<
// 			token.SHR, // >>
//
// 			token.NEQ, // !=
// 			token.LEQ, // <=
// 			token.GEQ, // >=
//
// 			token.LAND, // &&
// 			token.LOR:  // ||
// 			return true
// 		}
// 		return false
// 	}
// E:
// 	for i := range p.ns {
// 		if i < 2 {
// 			continue
// 		}
// 		if isOp(p.ns[i-2].tok) && p.ns[i-1].tok == NEW_LINE &&
// 			(p.ns[i].tok == token.ADD || p.ns[i].lit == "$") {
// 			p.ns = append(p.ns[:i-1], p.ns[i+1:]...)
// 			goto E
// 		}
// 	}
// 	for i := range p.ns {
// 		if i < 1 {
// 			continue
// 		}
// 		if p.ns[i-1].tok == NEW_LINE && p.ns[i].tok == token.ADD {
// 			p.ns = append(p.ns[:i-1], p.ns[i+1:]...)
// 			goto E
// 		}
// 	}
// 	for i := range p.ns {
// 		if i < 1 {
// 			continue
// 		}
// 		if isOp(p.ns[i-1].tok) && p.ns[i].tok == NEW_LINE {
// 			p.ns = append(p.ns[:i], p.ns[i+1:]...)
// 			goto E
// 		}
// 	}
//
// 	// Replase ELSEIF to ELSE IF
// 	// elseif:
// 	// 	for i := range p.ns {
// 	// 		if p.ns[i].tok == token.IDENT && strings.ToUpper(p.ns[i].lit) == "ELSEIF" {
// 	// 			var comb []node
// 	// 			comb = append(comb, p.ns[:i]...)
// 	// 			comb = append(comb, []node{
// 	// 				node{tok: token.ELSE, lit: "ELSE"},
// 	// 				node{tok: token.IF, lit: "IF"},
// 	// 			}...)
// 	// 			comb = append(comb, p.ns[i+1:]...)
// 	// 			p.ns = comb
// 	// 			goto elseif
// 	// 		}
// 	// 	}
//
// 	return
// }

func a(ns []node) (out string) {
	for _, n := range ns {
		switch n.tok {
		case NEW_LINE:
			out += fmt.Sprintf("\n")
		default:
			out += fmt.Sprintf(" %v", n.lit)
		}
	}
	return
}

func parse(b []byte) (ast goast.File, err []error) {
	// p.prepare()

	var p parser
	// var buf bytes.Buffer
	// p.logger = log.New(&buf, "f4go log:", log.Lshortfile)
	p.logger = log.New(os.Stdout, "f4go log:", log.Lshortfile)

	l := scanT(b)
	for e := l.Front(); e != nil; e = e.Next() {
		p.ns = append(p.ns, node{
			tok: e.Value.(*ele).tok,
			lit: string(e.Value.(*ele).b),
		})
	}
	lv(l)

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

	p.logger.Println("start of parseNodes")
	defer func() { p.logger.Println("end of parseNodes") }()

	if p.ident < 0 || p.ident >= len(p.ns) {
		p.errs = append(p.errs,
			fmt.Errorf("Ident is outside nodes: %d/%d", p.ident, len(p.ns)))
		return
	}

	// find all names of FUNCTION, SUBROUTINE, PROGRAM
	var internalFunction []string
	for ; p.ident < len(p.ns); p.ident++ {
		switch p.ns[p.ident].tok {
		case SUBROUTINE: // SUBROUTINE
			p.expect(SUBROUTINE)
			p.ident++
			p.expect(token.IDENT)
			internalFunction = append(internalFunction, p.ns[p.ident].lit)
		case PROGRAM:
			p.expect(PROGRAM)
			p.ident++
			p.expect(token.IDENT)
			internalFunction = append(internalFunction, p.ns[p.ident].lit)
		}
		for i := p.ident; i < len(p.ns) && p.ns[i].tok != NEW_LINE; i++ {
			if p.ns[p.ident].tok == FUNCTION {
				p.expect(FUNCTION)
				p.ident++
				p.expect(token.IDENT)
				internalFunction = append(internalFunction, p.ns[p.ident].lit)
			}
		}
	}
	p.ident = 0

	for ; p.ident < len(p.ns); p.ident++ {

		p.logger.Printf("parseNodes: pos = %d\t%v", p.ident, p.ns[p.ident])

		p.init()
		p.functionExternalName = append(p.functionExternalName,
			internalFunction...)

		switch p.ns[p.ident].tok {
		case NEW_LINE:
			p.ident++ // TODO
			continue
		case token.COMMENT:
			p.ident++ // TODO
			continue
		case SUBROUTINE: // SUBROUTINE
			var decl goast.Decl
			decl = p.parseSubroutine()
			decls = append(decls, decl)
			continue
		case PROGRAM: // PROGRAM
			var decl goast.Decl
			decl = p.parseProgram()
			decls = append(decls, decl)
			continue
		default:
			// Example :
			//  COMPLEX FUNCTION CDOTU ( N , CX , INCX , CY , INCY )
			for i := p.ident; i < len(p.ns) && p.ns[i].tok != NEW_LINE; i++ {
				if p.ns[i].tok == FUNCTION {
					var decl goast.Decl
					decl = p.parseFunction()
					decls = append(decls, decl)
					continue
				}
			}
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
		var comb []node
		comb = append(comb, p.ns[:p.ident]...)
		comb = append(comb, []node{
			node{tok: NEW_LINE, lit: "\n"},
			node{tok: PROGRAM, lit: "PROGRAM"},
			node{tok: token.IDENT, lit: "MAIN"},
			node{tok: NEW_LINE, lit: "\n"},
		}...)
		comb = append(comb, p.ns[p.ident:]...)
		p.ns = comb
		p.ident -= 1

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
		line += " " + p.ns[p.ident].lit
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

	p.logger.Println("start of parseFunction")
	defer func() { p.logger.Println("end of parseFunction") }()

	var fd goast.FuncDecl
	fd.Type = &goast.FuncType{
		Params: &goast.FieldList{},
	}

	var returnType []node
	for ; p.ns[p.ident].tok != FUNCTION && p.ns[p.ident].tok != NEW_LINE; p.ident++ {
		returnType = append(returnType, p.ns[p.ident])
	}
	p.expect(FUNCTION)

	p.ident++
	p.expect(token.IDENT)
	name := p.ns[p.ident].lit
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

	p.logger.Println("start of parseProgram")
	defer func() { p.logger.Println("end of parseProgram") }()

	p.expect(PROGRAM)

	p.ns[p.ident].tok = SUBROUTINE

	return p.parseSubroutine()
}

// Example :
//  SUBROUTINE CHBMV ( UPLO , N , K , ALPHA , A , LDA , X , INCX , BETA , Y , INCY )
func (p *parser) parseSubroutine() (decl goast.Decl) {

	p.logger.Println("start of parseSubroutine")
	defer func() { p.logger.Println("end of parseSubroutine") }()

	var fd goast.FuncDecl
	fd.Type = &goast.FuncType{
		Params: &goast.FieldList{},
	}

	p.expect(SUBROUTINE)

	p.ident++
	p.expect(token.IDENT)
	name := p.ns[p.ident].lit
	fd.Name = goast.NewIdent(name)
	defer func() {
		p.logger.Println("SUBROUTINE name is ", name)
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
		panic(fmt.Errorf("Expect %s, but we have {{%s,%s}}",
			view(t), view(p.ns[p.ident].tok), p.ns[p.ident].lit))
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

		p.logger.Printf("parseListStmt: pos = %d `%v` `%v`", p.ident,
			view(p.ns[p.ident].tok),
			p.ns[p.ident].lit)

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
func (p *parser) parseType(nodes []node) (typ string) {

	typ = "int"
	switch nodes[0].tok {
	case INTEGER:
		typ = "int"
	case LOGICAL:
		typ = "bool"
	case CHARACTER:
		typ = "byte"
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
		p.addError("Cannot parse type format: " + nodes[0].lit)
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

	var arraySize int = 1
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
	var baseType []node
	for ; p.ns[p.ident].tok != token.IDENT; p.ident++ {
		baseType = append(baseType, p.ns[p.ident])
	}
	p.expect(token.IDENT)

	var name string
	var additionType []node
	for ; p.ns[p.ident].tok != NEW_LINE &&
		p.ns[p.ident].tok != token.EOF; p.ident++ {
		// parse name
		p.expect(token.IDENT)
		name = p.ns[p.ident].lit

		// parse addition type
		additionType = make([]node, 0, 0)
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
	p.expect(token.IDENT)
	name := p.ns[p.ident].lit

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
	p.ident++

	sDo.Body = &goast.BlockStmt{
		Lbrace: 1,
		List:   p.parseListStmt(),
	}

	return
}

func (p *parser) parseIf() (sIf goast.IfStmt) {

	p.logger.Println("start of parseIf")
	defer func() { p.logger.Println("end of parseIf") }()

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
		p.ident--
		return
	}

	if p.ident >= len(p.ns) {
		p.logger.Printf("parseIf: outside of nodes")
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
			name := p.ns[p.ident].lit
			p.functionExternalName = append(p.functionExternalName, name)
			// fmt.Println("Function external: ", name)
		case token.COMMA:
			// ingore
		default:
			p.addError("Cannot parse External " + p.ns[p.ident].lit)
		}
	}
}

func (p *parser) parseStmt() (stmts []goast.Stmt) {

	defer func() {
		if r := recover(); r != nil {
			p.addError(fmt.Sprintf("%v", r))
		}
	}()

	p.logger.Printf("parseStmt: %v\tident pos = %d", p.ns[p.ident], p.ident)

	switch p.ns[p.ident].tok {
	case INTEGER, CHARACTER, COMPLEX, LOGICAL, REAL, DOUBLE:
		stmts = append(stmts, p.parseInit()...)

	case token.RETURN:
		stmts = append(stmts, &goast.ReturnStmt{})
		p.ident++

		p.expect(NEW_LINE)
		p.ident++

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
					p.ns[p.ident].lit)
			case token.COMMA:
				// ignore
			case INTEGER, CHARACTER, COMPLEX, LOGICAL, REAL:
				// type convertion - ignore
			default:
				p.addError("Cannot parse function name in INTRINSIC:" +
					p.ns[p.ident].lit)
			}
		}
		p.expect(NEW_LINE)

	case DATA:
		// Example:
		// DATA GAM , GAMSQ , RGAMSQ / 4096.D0 , 16777216.D0 , 5.9604645D-8 /
		sData := p.parseData()
		stmts = append(stmts, sData...)

	case WRITE:
		// TODO: add support WRITE
		var nodes []node
		for ; p.ident < len(p.ns); p.ident++ {
			if p.ns[p.ident].tok == NEW_LINE || p.ns[p.ident].tok == token.EOF {
				break
			}
			nodes = append(nodes, p.ns[p.ident])
		}
		p.addError("WRITE is not support.\n" + nodesToString(nodes))

	case STOP:
		p.expect(STOP)
		p.ident++
		p.expect(NEW_LINE)
		stmts = append(stmts, &goast.ReturnStmt{})

	case token.INT:
		// TODO: add support INT
		var nodes []node
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
			id := p.ns[p.ident].lit
			field := &goast.Field{
				Names: []*goast.Ident{goast.NewIdent(id)},
				Type:  goast.NewIdent("int"),
			}
			fields = append(fields, field)
		case token.RPAREN:
			p.ident--
			exit = true
		default:
			p.addError("Cannot parse parameter decl " + p.ns[p.ident].lit)
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
			names = append(names, p.ns[p.ident].lit)
		case token.COMMA:
			// ignore
		case token.QUO: // /
			exit = true
		default:
			p.addError("Cannot parse name in Data :" + p.ns[p.ident].lit)
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
			p.addError("Cannot parse value in Data :" + p.ns[p.ident].lit)
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
