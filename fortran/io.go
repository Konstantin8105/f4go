package fortran

import (
	"bytes"
	"fmt"
	goast "go/ast"
	goparser "go/parser"
	"go/token"
	"strconv"
	"unicode"
)

// Example:
//  REWIND NTRA
//  REWIND MSTP(161)
func (p *parser) parseRewind() (stmts []goast.Stmt) {
	p.expect(ftRewind)
	p.ident++

	var name string
	for ; p.ident < len(p.ns) && p.ns[p.ident].tok != ftNewLine; p.ident++ {
		name += string(p.ns[p.ident].b)
	}
	p.expect(ftNewLine)

	p.addImport("github.com/Konstantin8105/f4go/intrinsic")

	s := fmt.Sprintf("intrinsic.REWIND(%s)", name)

	ast, err := goparser.ParseExpr(s)
	if err != nil {
		panic(fmt.Errorf("pos:%v\nSource : %v\n Error: %v", p.ns[p.ident].pos, s, err))
	}

	return append(stmts, &goast.ExprStmt{
		X: ast,
	})
}

// Example:
//  WRITE ( * , FMT = 9999 ) SRNAME ( 1 : LEN_TRIM ( SRNAME ) ) , INFO
//  9999 FORMAT ( ' ** On entry to ' , A , ' parameter number ' , I2 , ' had ' , 'an illegal value' )
//
// write (*, '(I1,A2,I1)') i,'YY',i
func (p *parser) parseWrite() (stmts []goast.Stmt) {
	{
		start := p.ident
		p.expect(ftWrite)
		p.ident++
		p.expect(token.LPAREN)

		// WRITE ( 1, *) R
		//       ========= this out
		args, end := separateArgsParen(p.ns[p.ident:])
		p.ident += end

		// Pattern:
		//  WRITE( UNIT = ..., FMT = ...)
		// Other parameters are ignored

		// Part : UNIT
		unit := string(args[0][0].b)
		if len(args[0]) == 3 {
			unit = string(args[0][2].b)
		}
		if unit == "*" {
			unit = "6"
		}

		// Part: FMT
		fmts := args[1][0]
		if len(args[1]) == 3 {
			fmts = args[1][2]
		}

		var fs string
		if fmts.tok == token.INT {
			line := p.getLineByLabel(fmts.b)
			fs = p.parseFormat(line[2:])
		} else if fmts.tok == token.MUL {
			// Example: *
			fs = "\" %v\\n\""
		} else {
			// Example :
			// '(A80)'
			ns := scan(fmts.b[2 : len(fmts.b)-2])
			fs = p.parseFormat(ns)
		}

		p.ns = append(p.ns[:start+2], append(scan([]byte(fmt.Sprintf("%v , %v )", unit, fs))), p.ns[p.ident:]...)...)

		p.ident = start
	}
	start := p.ident
	p.ns = append(p.ns[:p.ident], append([]node{{tok: ftCall, b: []byte("call")}}, p.ns[p.ident:]...)...)

	p.ident++
	p.expect(ftWrite)
	p.ns[p.ident].tok = token.IDENT
	p.ns[p.ident].b = []byte("intrinsic.WRITE")
	p.ident++
	p.expect(token.LPAREN)

	counter := 0
	for ; ; p.ident++ {
		if p.ns[p.ident].tok == token.LPAREN {
			counter++
		}
		if p.ns[p.ident].tok == token.RPAREN {
			counter--
		}
		if counter == 0 {
			break
		}
	}
	p.ns[p.ident].tok = token.COMMA
	p.ns[p.ident].b = []byte(",")
	for ; ; p.ident++ {
		if p.ns[p.ident].tok == ftNewLine {
			break
		}
	}
	p.ns = append(p.ns[:p.ident], append([]node{
		{tok: token.RPAREN, b: []byte(")")},
	}, p.ns[p.ident:]...)...)

	p.ident = start

	return p.parseStmt()
}

func (p *parser) getLineByLabel(label []byte) (fs []node) {

	// memorization of FORMAT lines
	if v, ok := p.formats[string(label)]; ok {
		return v
	}

	var found bool
	var st int
	for st = p.ident; st < len(p.ns); st++ {
		if p.ns[st-1].tok == ftNewLine && bytes.Equal(p.ns[st].b, label) {
			found = true
			break
		}
	}
	if !found {
		p.addError("Cannot found label :" + string(label))
		return
	}

	for i := st; i < len(p.ns) && p.ns[i].tok != ftNewLine; i++ {
		fs = append(fs, p.ns[i])
		// remove line
		p.ns[i].tok, p.ns[i].b = ftNewLine, []byte("\n")
	}

	p.formats[string(label)] = fs

	return
}

func (p *parser) parseFormat(in []node) (s string) {
	var fs []node
	fs = append(fs, in...)
	if len(fs) == 0 {
		s = "\"\\n\""
		return
	}
	// From:
	//  ... / ...
	// To:
	//  ... , "\\n" , ...
	for i := 0; i < len(fs); i++ {
		if fs[i].tok != token.QUO { // not /
			continue
		}
		fs = append(fs[:i], append([]node{
			{tok: token.COMMA, b: []byte(",")},
			{tok: token.STRING, b: []byte("\\n")},
			{tok: token.COMMA, b: []byte(",")},
		}, fs[i+1:]...)...)
	}

	for i := 0; i < len(fs); i++ {
		f := fs[i]
		switch f.tok {
		case token.IDENT, token.COMMENT:
			switch byte(unicode.ToUpper(rune(f.b[0]))) {
			case 'I':
				s += "%" + string(f.b[1:]) + "d"
			case 'F', 'G', 'P':
				s += "%" + string(f.b[1:])
				if i+1 < len(fs) && fs[i+1].tok == token.PERIOD {
					i += 1
					s += "."
					if i+1 < len(fs) && fs[i+1].tok == token.INT {
						s += string(fs[i+1].b)
						i += 1
					}
				}
				s += "f"
			case 'E', 'D':
				s += "%" + string(f.b[1:])
				if i+1 < len(fs) && fs[i+1].tok == token.PERIOD {
					i += 1
					s += "."
					if i+1 < len(fs) && fs[i+1].tok == token.INT {
						s += string(fs[i+1].b)
						i += 1
					}
				}
				s += "E"
			case 'A':
				if string(f.b) == "A1" {
					s += "%c"
				} else {
					if len(f.b) > 1 {
						s += "%" + string(f.b[1:]) + "s"
					} else {
						s += "%s"
					}
				}

			case 'L':
				v, _ := strconv.Atoi(string(f.b[1:]))
				for i := 0; i < v; i++ {
					s += " "
				}
				s += "%t"

			default:
				p.addError(fmt.Sprintf(
					"Not support format pos = %v: %s", in[0].pos, f))
			}
		case token.INT:
			// 1X
			// 5X
			v, _ := strconv.Atoi(string(f.b))
			if byte(unicode.ToUpper(rune(fs[i+1].b[0]))) == 'X' {
				for i := 0; i < v; i++ {
					s += " "
				}
				i++
			}

		case token.STRING:
			str := string(f.b)
			if str[0] == '"' || str[0] == '\'' {
				s += str[1 : len(str)-1]
			} else {
				s += str
			}
		case token.COMMA, token.LPAREN, token.RPAREN:
			// ignore
		default:
			s += "%v"
		}
	}
	return "\"" + s + "\\n\""
}

// Example:
//  READ ( NIN , FMT = * ) TSTERR
//  READ ( NIN , FMT = * ) THRESH
//  READ ( NIN , FMT = * ) ( IDIM ( I ) , I = 1 , NIDIM )
func (p *parser) parseRead() (stmts []goast.Stmt) {
	p.expect(ftRead)
	p.ns[p.ident].tok = ftWrite

	stmts = p.parseStmt()

	if e, ok := stmts[0].(*goast.ExprStmt); ok {
		if c, ok := e.X.(*goast.CallExpr); ok {
			if sel, ok := c.Fun.(*goast.SelectorExpr); ok {
				sel.Sel.Name = "READ"
			}
		}
	}

	return
}

// Example:
//  OPEN ( NTRA , FILE = SNAPS )
//  OPEN ( NOUT , FILE = SUMMRY , STATUS = 'UNKNOWN' )
//  OPEN ( UNIT = 2 , FILE = "./testdata/main.f" )
func (p *parser) parseOpen() (stmts []goast.Stmt) {
	p.expect(ftOpen)
	p.ident++
	p.expect(token.LPAREN)
	args, end := separateArgsParen(p.ns[p.ident:])
	p.ident += end

	// Pattern:
	//  OPEN( UNIT = ..., FILE = ...)
	// Other parameters are ignored

	// Part : UNIT
	unit := string(args[0][0].b)
	if len(args[0]) == 3 {
		unit = string(args[0][2].b)
	}

	// Part : FILE
	file := string(args[1][0].b)
	if len(args[1]) == 3 {
		file = string(args[1][2].b)
	}

	s := fmt.Sprintf("intrinsic.OPEN(%s,%s)", unit, file)
	p.addImport("github.com/Konstantin8105/f4go/intrinsic")
	ast, err := goparser.ParseExpr(s)
	if err != nil {
		panic(fmt.Errorf("%s:%s", s, err))
	}
	stmts = append(stmts, &goast.ExprStmt{
		X: ast,
	})

	return
}

// Example:
//  CLOSE ( 2 )
//  CLOSE ( NIN )
func (p *parser) parseClose() (stmts []goast.Stmt) {
	p.expect(ftClose)
	p.ident++
	p.expect(token.LPAREN)
	args, end := separateArgsParen(p.ns[p.ident:])
	p.ident += end

	s := fmt.Sprintf("intrinsic.CLOSE(%s)", string(args[0][0].b))
	p.addImport("github.com/Konstantin8105/f4go/intrinsic")
	ast, err := goparser.ParseExpr(s)
	if err != nil {
		panic(err)
	}
	stmts = append(stmts, &goast.ExprStmt{
		X: ast,
	})

	return
}

//  READ  ( NIN , FMT = * ) ( IDIM ( I ) , I = 1 , NIDIM )
//  WRITE ( NIN , FMT = * ) ( IDIM ( I ) , I = 1 , NIDIM )
//  WRITE ( NIN , FMT = * ) ( A ( I , J ) , J = 1  ,  NIDIM )
//  ======================= this is ast
//                          ==============================
//                           this is return ForStmt
//
//                I = 1  NIDIM
//                I = 1  NIDIM
//                =============== loop
//                0 1 2  3
func (p parser) createForArguments(loop []node, ast goast.Expr) (f goast.ForStmt) {
	return goast.ForStmt{
		Init: &goast.AssignStmt{
			Lhs: []goast.Expr{goast.NewIdent(string(loop[0].b))},
			Tok: token.ASSIGN,
			Rhs: []goast.Expr{p.parseExprNodes([]node{loop[2]})},
		},
		Cond: p.parseExprNodes([]node{
			loop[0],
			{tok: token.LEQ, b: []byte("<=")},
			loop[3],
		}),
		Post: &goast.IncDecStmt{
			X:   goast.NewIdent(string(loop[0].b)),
			Tok: token.INC,
		},
		Body: &goast.BlockStmt{
			List: []goast.Stmt{
				&goast.ExprStmt{X: ast},
			},
		},
	}
}
