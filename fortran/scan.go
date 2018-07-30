package fortran

import (
	"bytes"
	"container/list"
	"fmt"
	"go/token"
)

type position struct {
	line int // line
	col  int // column
}

type ele struct {
	tok token.Token
	b   []byte
	pos position
}

func (e *ele) Split() (eles []ele) {
	var b []byte
	b = append(b, e.b...)

	var offset int
	for {
		if len(b) == 0 {
			break
		}

		var ind int
		for ind = 0; ind < len(b); ind++ {
			if b[ind] != ' ' {
				break
			}
		}

		var end int
		for end = ind; end < len(b) && b[end] != ' '; end++ {
		}

		if end-ind == 0 {
			break
		} else {
			eles = append(eles, ele{
				tok: e.tok,
				pos: position{
					line: e.pos.line,
					col:  e.pos.col + ind + offset,
				},
				b: b[ind:end],
			})
		}
		if end >= len(b) {
			break
		}
		b = b[end:]
		offset += end
	}

	return
}

// scanner represents a lexical scanner.
type elScan struct {
	eles *list.List
}

// newScanner returns a new instance of Scanner.
func scanT(b []byte) *list.List {
	var s elScan
	s.eles = list.New()
	s.eles.PushFront(&ele{
		tok: undefine,
		b:   b,
		pos: position{
			line: 1,
			col:  1,
		},
	})

	// separate lines
	s.scanBreakLines()

	// separate comments
	s.scanComments()

	// separate strings
	s.scanStrings()

	// preprocessor: add specific spaces
	s.preprocessor()
	defer func() {
		// postprocessor
		s.postprocessor()
	}()

	// separate on other token
	s.scanTokens()

	// remove empty
	s.scanEmpty()

	// scan numbers
	s.scanNumbers()

	// remove empty
	s.scanEmpty()

	s.scanTokensAfter()

	// remove empty
	s.scanEmpty()

	// IDENT for undefine
	for e := s.eles.Front(); e != nil; e = e.Next() {
		switch e.Value.(*ele).tok {
		case undefine:
			e.Value.(*ele).tok = token.IDENT
		}
	}

	return s.eles
}

// separate break lines
func (s *elScan) scanBreakLines() {
	for e := s.eles.Front(); e != nil; e = e.Next() {
		switch e.Value.(*ele).tok {
		case NEW_LINE:
			// ignore
		default:
			for j, ch := range e.Value.(*ele).b {
				if ch != '\n' {
					continue
				}
				s.extract(j, j+1, e, NEW_LINE)
				break
			}
		}
	}
	line := 1
	for e := s.eles.Front(); e != nil; e = e.Next() {
		e.Value.(*ele).pos.line = line
		e.Value.(*ele).pos.col = 1
		if e.Value.(*ele).tok == NEW_LINE {
			line++
		}
	}
}

// separate comments
func (s *elScan) scanComments() {
	for e := s.eles.Front(); e != nil; e = e.Next() {
		switch e.Value.(*ele).tok {
		case undefine:
			for j, ch := range e.Value.(*ele).b {
				var p bool

				// comments single line started from letters:
				// 'C', 'c', '*', 'd', 'D'
				if (ch == 'C' || ch == 'c' ||
					ch == '*' ||
					ch == 'D' || ch == 'd') &&
					(j == 0 || e.Value.(*ele).b[j-1] == '\n') {
					p = true
				}

				// comments inside line : '!'
				if ch == '!' {
					p = true
				}

				if p {
					b := e.Value.(*ele).b

					// find end of line include line break
					var end int
					for end = j; end < len(b) && b[end] != '\n'; end++ {
					}
					s.extract(j, end, e, token.COMMENT)
					break
				}
			}
		}
	}
}

// extract
// start - column started  (included)
// end   - column finished (not included)
func (s *elScan) extract(start, end int, e *list.Element, tok token.Token) {
	b := e.Value.(*ele).b

	if start == end {
		panic(fmt.Errorf("undefine symbol {%v,%v}", start, end))
	}

	if end > len(b) {
		panic(fmt.Errorf("outside of slice {%v,%v}", end, len(b)))
	}

	if start == 0 && end == len(b) {
		e.Value.(*ele).tok = tok
		return
	}

	if start == 0 { // comment at the first line
		present, aft := b[:end], b[end:]

		e.Value.(*ele).b = present
		e.Value.(*ele).tok = tok

		if len(aft) > 0 {
			s.eles.InsertAfter(&ele{
				tok: undefine,
				b:   aft,
				pos: position{
					line: e.Value.(*ele).pos.line,
					col:  e.Value.(*ele).pos.col + start,
				},
			}, e)
		}
		return
	}

	if end == len(b) {
		// start is not 0
		// end is end of slice
		bef, present := b[:start], b[start:]
		s.eles.InsertAfter(&ele{
			tok: tok,
			b:   present,
			pos: position{
				line: e.Value.(*ele).pos.line,
				col:  e.Value.(*ele).pos.col + start,
			},
		}, e)
		e.Value.(*ele).tok = undefine
		e.Value.(*ele).b = bef
		return
	}

	// start is not 0
	// end is not end

	bef, present, aft := b[:start], b[start:end], b[end:]

	e.Value.(*ele).tok = undefine
	e.Value.(*ele).b = bef

	pre := s.eles.InsertAfter(&ele{
		tok: tok,
		b:   present,
		pos: position{
			line: e.Value.(*ele).pos.line,
			col:  e.Value.(*ele).pos.col + start,
		},
	}, e)
	s.eles.InsertAfter(&ele{
		tok: undefine,
		b:   aft,
		pos: position{
			line: pre.Value.(*ele).pos.line,
			col:  pre.Value.(*ele).pos.col + start,
		},
	}, pre)
}

// separate strings
func (s *elScan) scanStrings() {
	for e := s.eles.Front(); e != nil; e = e.Next() {
		switch e.Value.(*ele).tok {
		case undefine:
			for j, ch := range e.Value.(*ele).b {
				if ch == '"' {
					b := e.Value.(*ele).b
					var end int
					for end = j + 1; end < len(b) && b[end] != '"'; end++ {
					}
					s.extract(j, end+1, e, token.STRING)
					break
				} else if ch == '\'' {
					b := e.Value.(*ele).b
					var end int
					for end = j + 1; end < len(b) && b[end] != '\''; end++ {
					}
					s.extract(j, end+1, e, token.STRING)
					break
				}
			}
		}
	}
}

// preprocessor for identification
func (s *elScan) preprocessor() {
	// Example of possible error:
	// IF ( 2.LE.1) ...
	//       |
	//       +- error here, because it is not value "2."
	//          it is value "2"

	ops := []string{
		".LT.",
		".GT.",
		".LE.",
		".GE.",
		".NOT.",
		".NE.",
		".EQ.",
		".AND.",
		".OR.",
		".TRUE.",
		".FALSE.",
	}

	for e := s.eles.Front(); e != nil; e = e.Next() {
		switch e.Value.(*ele).tok {
		case undefine:
			for _, op := range ops {
				e.Value.(*ele).b = bytes.Replace(
					[]byte(string(e.Value.(*ele).b)),
					[]byte(op),
					[]byte(" "+op+" "),
					-1)
			}
		}
	}

	for e := s.eles.Front(); e != nil; e = e.Next() {
		switch e.Value.(*ele).tok {
		case undefine:
			// Replase ELSEIF to ELSE IF
			e.Value.(*ele).b = bytes.Replace(
				[]byte(string(e.Value.(*ele).b)),
				[]byte("ELSEIF"),
				[]byte("ELSE IF"),
				-1)
			// Replace ENDDO to END
			e.Value.(*ele).b = bytes.Replace(
				[]byte(string(e.Value.(*ele).b)),
				[]byte("ENDDO"),
				[]byte("END"),
				-1)
			// Replace ENDIF to END
			e.Value.(*ele).b = bytes.Replace(
				[]byte(string(e.Value.(*ele).b)),
				[]byte("ENDIF"),
				[]byte("END"),
				-1)
		}
	}
}

// postprocessor
func (s *elScan) postprocessor() {

	// From:
	//  END SUBROUTINE
	//  END IF
	// To:
	//  END
	for e := s.eles.Front(); e != nil; e = e.Next() {
		if e.Value.(*ele).tok == END {
			for n := e.Next(); n != nil; n = e.Next() {
				if n.Value.(*ele).tok != NEW_LINE {
					s.eles.Remove(n)
				} else {
					break
				}
			}
		}
	}

	// Multiline function arguments
	// From:
	//  9999 FORMAT ( ' ** On entry to ' , A , ' parameter number ' , I2 , ' had ' ,
	//  'an illegal value' )
	// To:
	//  9999 FORMAT ( ' ** On entry to ' , A , ' parameter number ' , I2 , ' had ' , 'an illegal value' )
	for e := s.eles.Front(); e != nil; e = e.Next() {
		if e.Value.(*ele).tok == token.COMMA {
			n := e.Next()
			if n == nil {
				continue
			}
			if n.Value.(*ele).tok == NEW_LINE {
				s.eles.Remove(n)
			}
		}
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
	for e := s.eles.Front(); e != nil; e = e.Next() {
		if e.Value.(*ele).tok == DO {
			n := e.Next()
			if n == nil {
				continue
			}
			if n.Value.(*ele).tok == token.INT {
				doLabels[string(n.Value.(*ele).b)]++
				s.eles.Remove(n)
			}
		}
	}
	for e := s.eles.Front(); e != nil; e = e.Next() {
		if e.Value.(*ele).tok == token.INT {
			n := e.Next()
			if n == nil {
				continue
			}
			if n.Value.(*ele).tok != token.CONTINUE {
				continue
			}
			// Example : 30 CONTINUE
			if v, ok := doLabels[string(e.Value.(*ele).b)]; ok {
				if v <= 0 {
					panic("Not acceptable")
				}
				e.Value.(*ele).tok, e.Value.(*ele).b = END, []byte("END")
				n.Value.(*ele).tok, n.Value.(*ele).b = NEW_LINE, []byte("\n")

				for j := 1; j < v; j++ {
					s.eles.InsertAfter(&ele{
						tok: NEW_LINE,
						b:   []byte("\n"),
						pos: n.Value.(*ele).pos,
					}, n)
					s.eles.InsertAfter(&ele{
						tok: END,
						b:   []byte("END"),
						pos: n.Value.(*ele).pos,
					}, n)
				}
			} else {
				panic(fmt.Errorf("Cannot found label number: %v",
					string(e.Value.(*ele).b)))
			}

		}
	}

	// replace string concatenation
	for e := s.eles.Front(); e != nil; e = e.Next() {
		if e.Value.(*ele).tok == STRING_CONCAT {
			e.Value.(*ele).tok, e.Value.(*ele).b = token.ADD, []byte("+")
		}
	}

	// Multiline expression
	// if any in column 6, then merge lines
multi:
	for e := s.eles.Front(); e != nil; e = e.Next() {
		if e.Value.(*ele).tok == NEW_LINE {
			n := e.Next()
			if n == nil {
				continue
			}
			if n.Value.(*ele).pos.col == 6 {
				s.eles.Remove(e)
				s.eles.Remove(n)
				goto multi
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
	for e := s.eles.Front(); e != nil; e = e.Next() {
		if e.Value.(*ele).tok != NEW_LINE {
			continue
		}
		e = e.Next()
		if e == nil {
			break
		}
		if e.Value.(*ele).tok != PARAMETER {
			continue
		}
		// replace PARAMETER to NEW_LINE
		n := e.Next()
		e.Value.(*ele).b, e.Value.(*ele).tok = []byte{'\n'}, NEW_LINE
		e = n
		// replace ( to NEW_LINE
		if e.Value.(*ele).tok != token.LPAREN {
			panic("is not LPAREN")
		}
		e.Value.(*ele).b, e.Value.(*ele).tok = []byte{'\n'}, NEW_LINE
		e = e.Next()
		// find end )
		counter := 1
		for ; e != nil; e = e.Next() {
			if e.Value.(*ele).tok == NEW_LINE {
				panic("NEW_LINE is not accepted")
				break
			}
			if e.Value.(*ele).tok == token.LPAREN {
				counter++
			}
			if e.Value.(*ele).tok == token.RPAREN {
				counter--
			}
			if counter == 1 && e.Value.(*ele).tok == token.COMMA {
				// replace , to NEW_LINE
				e.Value.(*ele).b, e.Value.(*ele).tok = []byte{'\n'}, NEW_LINE
			}
			if counter == 0 {
				if e.Value.(*ele).tok != token.RPAREN {
					panic("Must RPAREN")
				}
				// replace ) to NEW_LINE
				e.Value.(*ele).b, e.Value.(*ele).tok = []byte{'\n'}, NEW_LINE
				break
			}
		}
	}
}

func (s *elScan) scanTokens() {
	entities := []struct {
		tok     token.Token
		pattern []string
	}{
		{tok: DOUBLE_COLON, pattern: []string{"::"}},
		{tok: token.COLON, pattern: []string{":"}},
		{tok: DOUBLE_STAR, pattern: []string{"**"}},
		{tok: token.MUL, pattern: []string{"*"}},
		{tok: STRING_CONCAT, pattern: []string{"//"}},
		// Operations
		{tok: token.COMMA, pattern: []string{","}},
		{tok: token.LPAREN, pattern: []string{"("}},
		{tok: token.RPAREN, pattern: []string{")"}},
		{tok: token.ASSIGN, pattern: []string{"="}},
		{tok: token.QUO, pattern: []string{"/"}},
		{tok: token.GTR, pattern: []string{">"}},
		{tok: token.LSS, pattern: []string{"<"}},
		{tok: DOLLAR, pattern: []string{"$"}},
		// Logicals
		{tok: token.LSS, pattern: []string{".LT."}},
		{tok: token.GTR, pattern: []string{".GT."}},
		{tok: token.LEQ, pattern: []string{".LE."}},
		{tok: token.GEQ, pattern: []string{".GE."}},
		{tok: token.NOT, pattern: []string{".NOT."}},
		{tok: token.NEQ, pattern: []string{".NE."}},
		{tok: token.EQL, pattern: []string{".EQ."}},
		{tok: token.LAND, pattern: []string{".AND."}},
		{tok: token.LOR, pattern: []string{".OR."}},
		{tok: token.IDENT, pattern: []string{".TRUE.", ".FALSE."}},
		// Other
		{tok: SUBROUTINE, pattern: []string{"SUBROUTINE"}},
		{tok: IMPLICIT, pattern: []string{"IMPLICIT"}},
		{tok: INTEGER, pattern: []string{"INTEGER"}},
		{tok: CHARACTER, pattern: []string{"CHARACTER"}},
		{tok: LOGICAL, pattern: []string{"LOGICAL"}},
		{tok: COMPLEX, pattern: []string{"COMPLEX"}},
		{tok: REAL, pattern: []string{"REAL"}},
		{tok: DATA, pattern: []string{"DATA"}},
		{tok: EXTERNAL, pattern: []string{"EXTERNAL"}},
		{tok: END, pattern: []string{"END", "ENDDO"}},
		{tok: DO, pattern: []string{"DO"}},
		{tok: DOUBLE, pattern: []string{"DOUBLE"}},
		{tok: FUNCTION, pattern: []string{"FUNCTION"}},
		{tok: token.IF, pattern: []string{"IF"}},
		{tok: token.ELSE, pattern: []string{"ELSE"}},
		{tok: token.CONTINUE, pattern: []string{"CONTINUE"}},
		{tok: CALL, pattern: []string{"CALL"}},
		{tok: THEN, pattern: []string{"THEN"}},
		{tok: token.RETURN, pattern: []string{"RETURN"}},
		{tok: WRITE, pattern: []string{"WRITE"}},
		{tok: WHILE, pattern: []string{"WHILE"}},
		{tok: PARAMETER, pattern: []string{"PARAMETER"}},
		{tok: PROGRAM, pattern: []string{"PROGRAM"}},
		{tok: PRECISION, pattern: []string{"PRECISION"}},
		{tok: INTRINSIC, pattern: []string{"INTRINSIC"}},
		{tok: FORMAT, pattern: []string{"FORMAT"}},
		{tok: STOP, pattern: []string{"STOP"}},
	}
A:
	var changed bool
	for e := s.eles.Front(); e != nil; e = e.Next() {
		for _, ent := range entities {
			for _, pat := range ent.pattern {
				switch e.Value.(*ele).tok {
				case undefine:
					index := bytes.Index([]byte(string(e.Value.(*ele).b)), []byte(pat))
					if index < 0 {
						continue
					}
					s.extract(index, index+len(pat), e, ent.tok)
					changed = true
					goto en
				}
			}
		}
	en:
	}
	if changed {
		goto A
	}
}

func (s *elScan) scanTokensAfter() {
	entities := []struct {
		tok     token.Token
		pattern []string
	}{
		{tok: token.PERIOD, pattern: []string{"."}},
		{tok: token.ADD, pattern: []string{"+"}},
		{tok: token.SUB, pattern: []string{"-"}},
	}
A:
	var changed bool
	for e := s.eles.Front(); e != nil; e = e.Next() {
		for _, ent := range entities {
			for _, pat := range ent.pattern {
				switch e.Value.(*ele).tok {
				case undefine:
					index := bytes.Index([]byte(string(e.Value.(*ele).b)), []byte(pat))
					if index < 0 {
						continue
					}
					s.extract(index, index+len(pat), e, ent.tok)
					changed = true
					goto en
				}
			}
		}
	en:
	}
	if changed {
		goto A
	}
}

// remove empty undefine tokens
func (s *elScan) scanEmpty() {
empty:
	var again bool
	for e := s.eles.Front(); e != nil; e = e.Next() {
		switch e.Value.(*ele).tok {
		case undefine:
			if len(e.Value.(*ele).b) == 0 {
				n := e.Next()
				s.eles.Remove(e)
				e = n
				again = true
				continue
			}
			if len(bytes.TrimSpace([]byte(string(e.Value.(*ele).b)))) == 0 {
				n := e.Next()
				s.eles.Remove(e)
				e = n
				again = true
				continue
			}
			es := e.Value.(*ele).Split()
			if len(es) == 1 && bytes.Equal(e.Value.(*ele).b, es[0].b) {
				continue
			}
			for i := len(es) - 1; i >= 1; i-- {
				s.eles.InsertAfter(&es[i], e)
			}
			if len(es) == 0 {
				s.eles.Remove(e)
				goto empty
			}
			e.Value.(*ele).b = es[0].b
			e.Value.(*ele).pos = es[0].pos
			goto empty
		}
	}
	if again {
		goto empty
	}
}

func (s *elScan) scanNumbers() {
numb:
	for e := s.eles.Front(); e != nil; e = e.Next() {
		switch e.Value.(*ele).tok {
		case undefine:
			// Examples:
			// +0.000E4
			// -44
			// 2
			// +123.213545Q-5
			// 12.324e34
			// 4E23
			// STAGES:        //
			//  1. Digits     // must
			//  2. Point      // must
			//  3. Digits     // maybe
			//  4. Exponenta  // maybe
			//  5. Sign       // maybe
			//  6. Digits     // maybe
			stage := 0
			for st := 0; st < len(e.Value.(*ele).b); st++ {
				if stage == 0 && isDigit(rune(e.Value.(*ele).b[st])) {
					var en int
					for en = st; en < len(e.Value.(*ele).b); en++ {
						if !isDigit(rune(e.Value.(*ele).b[en])) {
							break
						}
					}
					if en < len(e.Value.(*ele).b) && (e.Value.(*ele).b[en] == '.' ||
						e.Value.(*ele).b[en] == 'E' || e.Value.(*ele).b[en] == 'e' ||
						e.Value.(*ele).b[en] == 'D' || e.Value.(*ele).b[en] == 'd' ||
						e.Value.(*ele).b[en] == 'Q' || e.Value.(*ele).b[en] == 'q') {
						// FLOAT
						isPoint := e.Value.(*ele).b[en] == '.'
						fmt.Println(">", isPoint)
						for en = en + 1; en < len(e.Value.(*ele).b); en++ {
							if !isDigit(rune(e.Value.(*ele).b[en])) {
								break
							}
						}
						s.extract(st, en, e, token.FLOAT)
						goto numb
					} else {
						// INT
						s.extract(st, en, e, token.INT)
						goto numb
					}
				}
			}
		}
	}
}
