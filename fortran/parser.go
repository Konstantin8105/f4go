package fortran

import (
	"fmt"
	goast "go/ast"
	"go/token"
)

type node struct {
	tok token.Token
	lit string
}

type parser struct {
	sc    *Scanner
	ast   goast.File
	ident int
	ns    []node

	functionExternalName []string
	initVars             []initialVar

	// import packeges
	pkgs map[string]bool

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

func (p *parser) prepare() (err error) {
	var last token.Token
	for {
		tok, lit := p.sc.Scan()
		if tok == token.EOF {
			break
		}

		if tok == token.COMMENT {
			continue
		}

		// From:
		//  END SUBROUTINE
		//  END IF
		// To:
		//  END
		if last == END && tok != NEW_LINE {
			for tok != token.EOF && tok != NEW_LINE && tok != token.ILLEGAL {
				tok, lit = p.sc.Scan()
			}
			p.ns = append(p.ns, node{
				tok: tok,
				lit: lit,
			})
			last = tok
			continue
		}

		if last == NEW_LINE && tok == NEW_LINE {
			continue
		}

		// Multiline function arguments
		// From:
		//  9999 FORMAT ( ' ** On entry to ' , A , ' parameter number ' , I2 , ' had ' ,
		//  'an illegal value' )
		// To:
		//  9999 FORMAT ( ' ** On entry to ' , A , ' parameter number ' , I2 , ' had ' , 'an illegal value' )
		if last == token.COMMA && tok == NEW_LINE {
			continue
		}

		p.ns = append(p.ns, node{
			tok: tok,
			lit: lit,
		})
		last = tok
	}

	if len(p.ns) > 0 && p.ns[0].tok == NEW_LINE {
		p.ns = p.ns[1:]
	}

	// Simplification DO
	//-------------
	// From:
	//  DO 40 J = 1 , N
	//  DO 30 I = 1 , M
	//  C ( I , J ) = BETA * C ( I , J )
	//  30 CONTINUE
	//  40 CONTINUE
	//
	// Or from:
	//  DO 30 J = 1 , N
	//  DO 30 I = 1 , M
	//  C ( I , J ) = BETA * C ( I , J )
	//  30 CONTINUE
	//
	//-------------
	// To:
	//  DO J = 1 , N
	//  DO I = 1 , M
	//  C ( I , J ) = BETA * C ( I , J )
	//  END
	//  END
	//-------------
	doLabels := map[string]int{}
doLabelAgain:
	for i := range p.ns {
		if i == 0 {
			continue
		}
		if p.ns[i-1].tok == DO && p.ns[i].tok == token.INT {
			doLabels[p.ns[i].lit]++
			p.ns = append(p.ns[:i], p.ns[i+1:]...)
			goto doLabelAgain
		}
	}
again:
	for i := range p.ns {
		if i == 0 {
			continue
		}
		if p.ns[i-1].tok == token.INT && p.ns[i].tok == token.CONTINUE {
			if v, ok := doLabels[p.ns[i-1].lit]; ok {
				if v <= 0 {
					panic("Not acceptable")
				}
				p.ns[i-1].tok, p.ns[i-1].lit = END, "end"
				p.ns[i].tok, p.ns[i].lit = NEW_LINE, "\n"

				var inject []node
				for j := 1; j < v; j++ {
					inject = append(inject, []node{
						node{
							tok: END,
							lit: "end",
						},
						node{
							tok: NEW_LINE,
							lit: "\n",
						},
					}...)
				}

				if len(inject) > 0 {
					inject = append(inject, p.ns[i+1:]...)
					p.ns = append(p.ns[:i+1], inject...)
					goto again
				}
			}
		}
	}

	// Simplification of PARAMETER:
	// From:
	//  PARAMETER ( ONE = ( 1.0E+0 , 0.0E+0 )  , ZERO = 0.0E+0 )
	// To:
	//  ONE = ( 1.0E+0 , 0.0E+0 )
	//  ZERO = 0.0E+0
	//
	for i := 0; i < len(p.ns); i++ {
		if p.ns[i].tok != PARAMETER {
			continue
		}
		// replace PARAMETER to NEW_LINE
		p.ns[i].tok, p.ns[i].lit = NEW_LINE, "\n"
		i++
		// replace ( to NEW_LINE
		if p.ns[i].tok != token.LPAREN {
			panic("is not LPAREN")
		}
		p.ns[i].tok, p.ns[i].lit = NEW_LINE, "\n"
		i++
		// find end )
		counter := 1
		for j := i; p.ns[j].tok != NEW_LINE; j, i = j+1, i+1 {
			if p.ns[j].tok == token.LPAREN {
				counter++
			}
			if p.ns[j].tok == token.RPAREN {
				counter--
			}
			if counter == 1 && p.ns[j].tok == token.COMMA {
				// replace , to NEW_LINE
				p.ns[i].tok, p.ns[i].lit = NEW_LINE, "\n"
			}
			if counter == 0 {
				break
			}
		}
		// replace ) to NEW_LINE
		if p.ns[i].tok != token.RPAREN {
			panic("is not RPAREN : " + view(p.ns[i].tok))
		}
		p.ns[i].tok, p.ns[i].lit = NEW_LINE, "\n"
		i++
	}

	// Multiline expression
	// From:
	//  IF ( ( M .EQ. 0 ) .OR. ( N .EQ. 0 ) .OR.
	//  + ( ( ( ALPHA .EQ. ZERO ) .OR. ( K .EQ. 0 ) ) .AND. ( BETA .EQ. ONE ) ) ) RETURN
	// To:
	//  IF ( ( M .EQ. 0 ) .OR. ( N .EQ. 0 ) .OR. ( ( ( ALPHA .EQ. ZERO ) .OR. ( K .EQ. 0 ) ) .AND. ( BETA .EQ. ONE ) ) ) RETURN
	//
	// From:
	//  NORM = SCALE * DSQRT ( ( CDABS ( CA / DCMPLX ( SCALE , 0.0d0 ) ) ) ** 2 +
	//  ( CDABS ( CB / DCMPLX ( SCALE , 0.0d0 ) ) ) ** 2 )
	// To:
	//  NORM = SCALE * DSQRT ( ( CDABS ( CA / DCMPLX ( SCALE , 0.0d0 ) ) ) ** 2 + ( CDABS ( CB / DCMPLX ( SCALE , 0.0d0 ) ) ) ** 2 )
	isOp := func(t token.Token) bool {
		switch t {
		case token.ADD, // +
			token.SUB, // -
			// token.MUL, // *
			// token.QUO, // /
			// token.REM, // %

			token.AND, // &
			token.OR,  // |
			token.XOR, // ^
			token.SHL, // <<
			token.SHR, // >>

			token.LAND, // &&
			token.LOR:  // ||
			return true
		}
		return false
	}
E:
	for i := range p.ns {
		if i < 2 {
			continue
		}
		if isOp(p.ns[i-2].tok) && p.ns[i-1].tok == NEW_LINE && p.ns[i].tok == token.ADD {
			p.ns = append(p.ns[:i-1], p.ns[i+1:]...)
			goto E
		}
	}
	for i := range p.ns {
		if i < 1 {
			continue
		}
		if p.ns[i-1].tok == NEW_LINE && p.ns[i].tok == token.ADD {
			p.ns = append(p.ns[:i-1], p.ns[i+1:]...)
			goto E
		}
	}
	for i := range p.ns {
		if i < 1 {
			continue
		}
		if isOp(p.ns[i-1].tok) && p.ns[i].tok == NEW_LINE {
			p.ns = append(p.ns[:i], p.ns[i+1:]...)
			goto E
		}
	}

	return
}

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

func (p *parser) parse() (err error) {
	err = p.prepare()
	if err != nil {
		return
	}

	p.ast.Name = goast.NewIdent("main")

	var decls []goast.Decl
	p.ident = 0
	decls = p.transpileToNode()
	p.showErrors()

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

	p.ast.Decls = append(p.ast.Decls, decls...)

	return
}

func (p *parser) showErrors() {
	fmt.Println("--------")
	fmt.Println("Errors:")
	if len(p.errs) > 0 {
		for index, e := range p.errs {
			fmt.Printf("[%3d]\t%v\n", index+1, e)
		}
		return
	}

	line := p.getLine()
	if line != "" {
		fmt.Println("Present line: ", line)
	}
}

func (p *parser) transpileToNode() (decls []goast.Decl) {

	if p.ident < 0 || p.ident >= len(p.ns) {
		p.errs = append(p.errs,
			fmt.Errorf("Ident is outside nodes: %d/%d", p.ident, len(p.ns)))
		return
	}

	switch p.ns[p.ident].tok {
	case SUBROUTINE:
		var decl goast.Decl
		decl = p.transpileSubroutine()
		decls = append(decls, decl)

	default:
		// move to next NEW_LINE
		p.addError("Cannot parse line: " + p.getLine())
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

func (p *parser) transpileSubroutine() (decl goast.Decl) {
	p.init()

	var fd goast.FuncDecl
	fd.Type = &goast.FuncType{
		Params: &goast.FieldList{},
	}

	p.expect(SUBROUTINE)

	p.ident++
	p.expect(token.IDENT)
	name := p.ns[p.ident].lit

	p.ident++
	var hasParens bool = p.ns[p.ident].tok == token.LPAREN
	if hasParens {
		p.expect(token.LPAREN)

		// Parameters
		p.ident++
		fd.Type.Params.List = p.parseParamDecl()

		p.ident++
		p.expect(token.RPAREN)

		p.ident++
		p.expect(NEW_LINE)
	}

	p.ident++
	fd.Name = goast.NewIdent(name)
	fd.Body = &goast.BlockStmt{
		Lbrace: 1,
		List:   p.transpileListStmt(),
	}
	p.ident++

	// delete external function type definition
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

	// add correct type of subroutine arguments
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

	// init vars
	var vars []goast.Stmt
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

	fd.Body.List = append(vars, fd.Body.List...)

	decl = &fd
	return
}

func (p *parser) addError(msg string) {
	last := p.ident
	defer func() {
		p.ident = last
	}()

	p.errs = append(p.errs, fmt.Errorf("%s\nCode line :%s", msg, p.getLine()))
}

func (p *parser) expect(t token.Token) {
	if t != p.ns[p.ident].tok {
		// Show all errors
		p.showErrors()
		// Panic
		panic(fmt.Errorf("Expect %s, but we have {{%s,%s}}",
			view(t), view(p.ns[p.ident].tok), p.ns[p.ident].lit))
	}
}

func (p *parser) transpileListStmt() (stmts []goast.Stmt) {
	for p.ident < len(p.ns) {
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
			// p.addError("stmt is nil in line : " + p.getLine())
			// break
			continue
		}
		stmts = append(stmts, stmt...)
	}
	return
}

func (p *parser) parseInit() (stmts []goast.Stmt) {

	// TODO: Example of function initialization
	// COMPLEX FUNCTION CDOTU ( N , CX , INCX , CY , INCY )
	// DOUBLE PRECISION FUNCTION DNRM2 ( N , X , INCX )
	// COMPLEX * 16 FUNCTION ZDOTC ( N , ZX , INCX , ZY , INCY )

	identType := "int"
	switch p.ns[p.ident].tok {
	case LOGICAL:
		identType = "bool"
	case CHARACTER:
		identType = "byte"
	case COMPLEX:
		identType = "complex128"
	case REAL:
		identType = "float64"
	}

	p.ident++
	var varPos int = -1
	for ; p.ns[p.ident].tok != NEW_LINE; p.ident++ {
		switch p.ns[p.ident].tok {
		case token.IDENT:
			p.initVars = append(p.initVars, initialVar{
				name: p.ns[p.ident].lit,
				typ:  identType,
			})
			varPos = len(p.initVars) - 1
		case token.LPAREN:
			// Fortran example: INTEGER A(*)
			p.expect(token.LPAREN)
			p.ident++
			fmt.Printf("Ignore in initialization : ")
			for ; p.ns[p.ident].tok != token.RPAREN; p.ident++ {
				// ignore inside () for example:
				// COMPLEX A ( LDA , * ) , X ( * ) , Y ( * )
				fmt.Printf(" %v", p.ns[p.ident].lit)
			}
			fmt.Printf("\n")
			if len(p.initVars) == 0 {
				p.addError("Cannot parse initVars , because len = 0")
				break
			}
			if varPos == -1 {
				p.addError("Undefine variable name")
				break
			}
			p.initVars[len(p.initVars)-1].typ =
				"[]" + p.initVars[len(p.initVars)-1].typ
		case token.COMMA:
			// ignore
		default:
			p.addError("Cannot parseInit value : " + p.ns[p.ident].lit)
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
		List:   p.transpileListStmt(),
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
		List:   p.transpileListStmt(),
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
			List:   p.transpileListStmt(),
		}
	} else {
		sIf.Body = &goast.BlockStmt{
			Lbrace: 1,
			List:   p.parseStmt(),
		}
	}

	if p.ns[p.ident].tok == token.ELSE {
		p.ident++
		if p.ns[p.ident].tok == token.IF {
			ifr := p.parseIf()
			sIf.Else = &ifr
		} else {
			sIf.Else = &goast.BlockStmt{
				Lbrace: 1,
				List:   p.transpileListStmt(),
			}
		}
	}

	return
}

func (p *parser) parseExpr(start, end int) (expr goast.Expr) {
	for i := start; i < end; i++ {
		if p.ns[i].tok == NEW_LINE {
			p.addError("NEW_LINE is not acceptable inside expression")
		}
	}

	return p.parseBinaryExpr(p.ns[start:end])
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
	switch p.ns[p.ident].tok {
	case INTEGER, CHARACTER, COMPLEX, LOGICAL, REAL:
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

		//TODO: add DATA
		// DATA GAM , GAMSQ , RGAMSQ / 4096.D0 , 16777216.D0 , 5.9604645D-8 /

	case WRITE:
		p.addError("WRITE is not support")
		for ; p.ident < len(p.ns); p.ident++ {
			if p.ns[p.ident].tok == NEW_LINE || p.ns[p.ident].tok == token.EOF {
				break
			}
		}

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
	return
}
