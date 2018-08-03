package fortran

import (
	"bytes"
	"container/list"
	"fmt"
	"go/token"
	"strings"
)

type position struct {
	line int // line
	col  int // column
}

type node struct {
	tok token.Token
	b   []byte
	pos position
}

func (e node) String() string {
	return fmt.Sprintf("[%v, `%s`, %v]", view(e.tok), string(e.b), e.pos)
}

func (e *node) Split() (nodes []node) {
	var b []byte
	b = append(b, e.b...)

	var offset int
	for {
		if len(b) == 0 {
			break
		}

		var st int
		for st = 0; st < len(b); st++ {
			if b[st] != ' ' {
				break
			}
		}

		var end int
		for end = st; end < len(b) && b[end] != ' '; end++ {
		}

		if end-st == 0 {
			break
		} else {
			nodes = append(nodes, node{
				tok: e.tok,
				pos: position{
					line: e.pos.line,
					col:  e.pos.col + st + offset,
				},
				b: b[st:end],
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
type scanner struct {
	nodes *list.List
}

func scan(b []byte) *list.List {
	var s scanner
	s.nodes = list.New()
	s.nodes.PushFront(&node{
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
	s.scanTokenWithPoint()
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
	for e := s.nodes.Front(); e != nil; e = e.Next() {
		switch e.Value.(*node).tok {
		case undefine:
			e.Value.(*node).tok = token.IDENT
		}
	}

	// token GO TO
	s.scanGoto()

	return s.nodes
}

// separate break lines
func (s *scanner) scanBreakLines() {
B:
	var again bool
	for e := s.nodes.Front(); e != nil; e = e.Next() {
		switch e.Value.(*node).tok {
		case NEW_LINE:
			// ignore
		default:
			for j := len(e.Value.(*node).b) - 1; j >= 0; j-- {
				if e.Value.(*node).b[j] != '\n' {
					continue
				}
				s.extract(j, j+1, e, NEW_LINE)
				again = true
			}
		}
	}
	if again {
		goto B
	}
	line := 1
	for e := s.nodes.Front(); e != nil; e = e.Next() {
		e.Value.(*node).pos.line = line
		e.Value.(*node).pos.col = 1
		if e.Value.(*node).tok == NEW_LINE {
			line++
		}
	}
}

// separate comments
func (s *scanner) scanComments() {
	// comments single line started from letters:
	// 'C', 'c', '*', 'd', 'D'
	for e := s.nodes.Front(); e != nil; e = e.Next() {
		switch e.Value.(*node).tok {
		case undefine:
			if len(e.Value.(*node).b) == 0 {
				continue
			}
			ch := e.Value.(*node).b[0]
			if ch == 'C' || ch == 'c' ||
				ch == '*' ||
				ch == 'D' || ch == 'd' {
				e.Value.(*node).tok = token.COMMENT
			}
		}
	}

	// comments inside line : '!'
Op:
	var again bool
	for e := s.nodes.Front(); e != nil; e = e.Next() {
		switch e.Value.(*node).tok {
		case undefine:
			if len(e.Value.(*node).b) == 0 {
				continue
			}
			var st int
			var found bool
			for st = 0; st < len(e.Value.(*node).b); st++ {
				if e.Value.(*node).b[st] == '!' {
					found = true
					break
				}
			}
			if found {
				s.extract(st, len(e.Value.(*node).b), e, token.COMMENT)
				again = true
			}
		}
	}
	if again {
		goto Op
	}
}

// extract
// start - column started  (included)
// end   - column finished (not included)
func (s *scanner) extract(start, end int, e *list.Element, tok token.Token) {
	b := e.Value.(*node).b

	if start == end {
		panic(fmt.Errorf("undefine symbol {%v,%v}", start, end))
	}

	if end > len(b) {
		panic(fmt.Errorf("outside of slice {%v,%v}", end, len(b)))
	}

	if start == 0 && end == len(b) {
		e.Value.(*node).tok = tok
		return
	}

	if start == 0 { // comment at the first line
		present, aft := b[:end], b[end:]

		e.Value.(*node).b = present
		e.Value.(*node).tok = tok

		if len(aft) > 0 {
			s.nodes.InsertAfter(&node{
				tok: undefine,
				b:   aft,
				pos: position{
					line: e.Value.(*node).pos.line,
					col:  e.Value.(*node).pos.col + start,
				},
			}, e)
		}
		return
	}

	if end == len(b) {
		// start is not 0
		// end is end of slice
		bef, present := b[:start], b[start:]
		s.nodes.InsertAfter(&node{
			tok: tok,
			b:   present,
			pos: position{
				line: e.Value.(*node).pos.line,
				col:  e.Value.(*node).pos.col + start,
			},
		}, e)
		e.Value.(*node).tok = undefine
		e.Value.(*node).b = bef
		return
	}

	// start is not 0
	// end is not end

	bef, present, aft := b[:start], b[start:end], b[end:]

	e.Value.(*node).tok = undefine
	e.Value.(*node).b = bef

	s.nodes.InsertAfter(&node{
		tok: undefine,
		b:   aft,
		pos: position{
			line: e.Value.(*node).pos.line,
			col:  e.Value.(*node).pos.col + end,
		},
	}, e)

	s.nodes.InsertAfter(&node{
		tok: tok,
		b:   present,
		pos: position{
			line: e.Value.(*node).pos.line,
			col:  e.Value.(*node).pos.col + start,
		},
	}, e)
}

// separate strings
func (s *scanner) scanStrings() {
	for e := s.nodes.Front(); e != nil; e = e.Next() {
		switch e.Value.(*node).tok {
		case undefine:
			for j, ch := range e.Value.(*node).b {
				if ch == '"' {
					b := e.Value.(*node).b
					var end int
					for end = j + 1; end < len(b) && b[end] != '"'; end++ {
					}
					s.extract(j, end+1, e, token.STRING)
					break
				} else if ch == '\'' {
					b := e.Value.(*node).b
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

// scanTokenWithPoint for identification
func (s *scanner) scanTokenWithPoint() {
	// Example of possible error:
	// IF ( 2.LE.1) ...
	//       |
	//       +- error here, because it is not value "2."
	//          it is value "2"

	entities := []struct {
		tok     token.Token
		pattern string
	}{
		// operation with points
		{tok: token.LSS, pattern: ".LT."},
		{tok: token.GTR, pattern: ".GT."},
		{tok: token.LEQ, pattern: ".LE."},
		{tok: token.GEQ, pattern: ".GE."},
		{tok: token.NOT, pattern: ".NOT."},
		{tok: token.NEQ, pattern: ".NE."},
		{tok: token.EQL, pattern: ".EQ."},
		{tok: token.LAND, pattern: ".AND."},
		{tok: token.LAND, pattern: ".AND ."}, //add for test other/sgelq.f
		{tok: token.LOR, pattern: ".OR."},
		{tok: token.IDENT, pattern: ".TRUE."},
		{tok: token.IDENT, pattern: ".FALSE."},

		// !=
		{tok: token.NEQ, pattern: "/="},
		// other
		{tok: DOUBLE_COLON, pattern: "::"},
		{tok: token.COLON, pattern: ":"},
		{tok: token.COMMA, pattern: ","},
		{tok: token.LPAREN, pattern: "("},
		{tok: token.RPAREN, pattern: ")"},
		{tok: token.ASSIGN, pattern: "="},
		{tok: token.GTR, pattern: ">"},
		{tok: token.LSS, pattern: "<"},
		{tok: DOLLAR, pattern: "$"},
		// stars
		{tok: DOUBLE_STAR, pattern: "**"},
		{tok: token.MUL, pattern: "*"},
		// devs
		{tok: STRING_CONCAT, pattern: "//"},
		{tok: token.QUO, pattern: "/"},
	}
	for _, ent := range entities {
		if !bytes.Equal([]byte(ent.pattern), bytes.ToUpper([]byte(ent.pattern))) {
			panic(fmt.Errorf("Not valid pattern: %s", ent.pattern))
		}
	}

A:
	var changed bool
	for e := s.nodes.Front(); e != nil; e = e.Next() {
		if e.Value.(*node).tok != undefine {
			continue
		}
		up := bytes.ToUpper(e.Value.(*node).b)
		for _, ent := range entities {
			ind := bytes.Index(
				up,
				[]byte(ent.pattern))
			if ind < 0 {
				continue
			}
			s.extract(ind, ind+len(ent.pattern), e, ent.tok)
			changed = true
			break
		}
	}
	if changed {
		goto A
	}
}

// postprocessor
func (s *scanner) postprocessor() {

	// From:
	//  END SUBROUTINE
	//  END IF
	// To:
	//  END
	for e := s.nodes.Front(); e != nil; e = e.Next() {
		if e.Value.(*node).tok == END {
			for n := e.Next(); n != nil; n = e.Next() {
				if n.Value.(*node).tok != NEW_LINE {
					s.nodes.Remove(n)
				} else {
					break
				}
			}
		}
	}

	// From:
	//   ELSEIF
	// To:
	//   ELSE IF
	for e := s.nodes.Front(); e != nil; e = e.Next() {
		if e.Value.(*node).tok == ELSEIF {
			e.Value.(*node).tok, e.Value.(*node).b = token.ELSE, []byte("ELSE")
			s.nodes.InsertAfter(&node{
				tok: token.IF,
				b:   []byte("IF"),
			}, e)
		}
	}

	// From:
	//   /= token.NEQ
	// To:
	//   != token.NEQ
	for e := s.nodes.Front(); e != nil; e = e.Next() {
		if e.Value.(*node).tok == token.NEQ {
			e.Value.(*node).tok, e.Value.(*node).b = token.NEQ, []byte("!=")
		}
	}

	// replace string concatenation
	for e := s.nodes.Front(); e != nil; e = e.Next() {
		if e.Value.(*node).tok == STRING_CONCAT {
			e.Value.(*node).tok, e.Value.(*node).b = token.ADD, []byte("+")
		}
	}

	// Multiline expression
	// if any in column 6, then merge lines
multi:
	for e := s.nodes.Front(); e != nil; e = e.Next() {
		if e.Value.(*node).tok == NEW_LINE {
			n := e.Next()
			if n == nil {
				continue
			}
			if n.Value.(*node).pos.col == 6 {
				s.nodes.Remove(e)
				s.nodes.Remove(n)
				goto multi
			}
		}
	}

	// Multiline function arguments
	// From:
	//  9999 FORMAT ( ' ** On entry to ' , A , ' parameter number ' , I2 , ' had ' ,
	//  'an illegal value' )
	// To:
	//  9999 FORMAT ( ' ** On entry to ' , A , ' parameter number ' , I2 , ' had ' , 'an illegal value' )
	for e := s.nodes.Front(); e != nil; e = e.Next() {
		if e.Value.(*node).tok == token.COMMA {
			n := e.Next()
			if n == nil {
				continue
			}
			if n.Value.(*node).tok == NEW_LINE {
				s.nodes.Remove(n)
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
	for e := s.nodes.Front(); e != nil; e = e.Next() {
		if e.Value.(*node).tok != NEW_LINE {
			continue
		}
		e = e.Next()
		if e == nil {
			break
		}
		if e.Value.(*node).tok != PARAMETER {
			continue
		}
		// replace PARAMETER to NEW_LINE
		n := e.Next()
		e.Value.(*node).b, e.Value.(*node).tok = []byte{'\n'}, NEW_LINE
		e = n
		// replace ( to NEW_LINE
		if e.Value.(*node).tok != token.LPAREN {
			panic("is not LPAREN")
		}
		e.Value.(*node).b, e.Value.(*node).tok = []byte{'\n'}, NEW_LINE
		e = e.Next()
		// find end )
		counter := 1
		for ; e != nil; e = e.Next() {
			if e.Value.(*node).tok == NEW_LINE {
				// panic(fmt.Errorf("NEW_LINE is not accepted"))
				break
			}
			if e.Value.(*node).tok == token.LPAREN {
				counter++
			}
			if e.Value.(*node).tok == token.RPAREN {
				counter--
			}
			if counter == 1 && e.Value.(*node).tok == token.COMMA {
				// replace , to NEW_LINE
				e.Value.(*node).b, e.Value.(*node).tok = []byte{'\n'}, NEW_LINE
			}
			if counter == 0 {
				if e.Value.(*node).tok != token.RPAREN {
					panic("Must RPAREN")
				}
				// replace ) to NEW_LINE
				e.Value.(*node).b, e.Value.(*node).tok = []byte{'\n'}, NEW_LINE
				break
			}
		}
	}

	// .TRUE. to true
	// .FALSE. to false
	for e := s.nodes.Front(); e != nil; e = e.Next() {
		if e.Value.(*node).tok != token.IDENT {
			continue
		}
		switch strings.ToUpper(string(e.Value.(*node).b)) {
		case ".TRUE.":
			e.Value.(*node).b = []byte("true")
		case ".FALSE.":
			e.Value.(*node).b = []byte("false")
		}
	}
}

func (s *scanner) scanTokens() {
	entities := []struct {
		tok     token.Token
		pattern []string
	}{
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
		{tok: token.GOTO, pattern: []string{"GOTO"}},
		{tok: ELSEIF, pattern: []string{"ELSEIF"}},
	}
	for _, ent := range entities {
		for _, pat := range ent.pattern {
			if !bytes.Equal([]byte(pat), bytes.ToUpper([]byte(pat))) {
				panic(fmt.Errorf("Not valid pattern: %s", pat))
			}
		}
	}
A:
	var changed bool
	for e := s.nodes.Front(); e != nil; e = e.Next() {
		for _, ent := range entities {
			if e.Value.(*node).tok != undefine {
				continue
			}
			up := bytes.ToUpper(e.Value.(*node).b)
			for _, pat := range ent.pattern {
				index := bytes.Index(
					up,
					[]byte(pat))
				if index < 0 {
					continue
				}

				var found bool
				if index == 0 {
					if len(e.Value.(*node).b) == len(pat) ||
						!(isLetter(rune(e.Value.(*node).b[len(pat)])) ||
							isDigit(rune(e.Value.(*node).b[len(pat)]))) {
						found = true
					}
				}
				if index > 0 {
					if e.Value.(*node).b[index-1] == ' ' &&
						(len(e.Value.(*node).b) == index+len(pat) ||
							!isLetter(rune(e.Value.(*node).b[index+len(pat)]))) {
						found = true
					}
				}

				if found {
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

func (s *scanner) scanTokensAfter() {
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
	for e := s.nodes.Front(); e != nil; e = e.Next() {
		for _, ent := range entities {
			for _, pat := range ent.pattern {
				switch e.Value.(*node).tok {
				case undefine:
					index := bytes.Index([]byte(string(e.Value.(*node).b)), []byte(pat))
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
func (s *scanner) scanEmpty() {
empty:
	var again bool
	for e := s.nodes.Front(); e != nil; e = e.Next() {
		switch e.Value.(*node).tok {
		case undefine:
			if len(e.Value.(*node).b) == 0 {
				n := e.Next()
				s.nodes.Remove(e)
				e = n
				again = true
				continue
			}
			if len(bytes.TrimSpace([]byte(string(e.Value.(*node).b)))) == 0 {
				n := e.Next()
				s.nodes.Remove(e)
				e = n
				again = true
				continue
			}
			es := e.Value.(*node).Split()
			if len(es) == 1 && bytes.Equal(e.Value.(*node).b, es[0].b) {
				continue
			}
			for i := len(es) - 1; i >= 1; i-- {
				s.nodes.InsertAfter(&es[i], e)
			}
			if len(es) == 0 {
				n := e.Next()
				s.nodes.Remove(e)
				e = n
				again = true
				continue
			}
			e.Value.(*node).b = es[0].b
			e.Value.(*node).pos = es[0].pos
			again = true
			continue
		}
	}
	if again {
		goto empty
	}
}

func (s *scanner) scanNumbers() {
numb:
	for e := s.nodes.Front(); e != nil; e = e.Next() {
		switch e.Value.(*node).tok {
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
			for st := 0; st < len(e.Value.(*node).b); st++ {
				if isDigit(rune(e.Value.(*node).b[st])) {
					var en int
					for en = st; en < len(e.Value.(*node).b); en++ {
						if !isDigit(rune(e.Value.(*node).b[en])) {
							break
						}
					}
					if en < len(e.Value.(*node).b) && (e.Value.(*node).b[en] == '.' ||
						e.Value.(*node).b[en] == 'E' || e.Value.(*node).b[en] == 'e' ||
						e.Value.(*node).b[en] == 'D' || e.Value.(*node).b[en] == 'd' ||
						e.Value.(*node).b[en] == 'Q' || e.Value.(*node).b[en] == 'q') {
						// FLOAT
						if e.Value.(*node).b[en] == '.' {
							for en = en + 1; en < len(e.Value.(*node).b); en++ {
								if !isDigit(rune(e.Value.(*node).b[en])) {
									break
								}
							}
						}
						if en < len(e.Value.(*node).b) &&
							(e.Value.(*node).b[en] == 'E' || e.Value.(*node).b[en] == 'e' ||
								e.Value.(*node).b[en] == 'D' || e.Value.(*node).b[en] == 'd' ||
								e.Value.(*node).b[en] == 'Q' || e.Value.(*node).b[en] == 'q') {
							if en+1 < len(e.Value.(*node).b) &&
								(e.Value.(*node).b[en+1] == '+' || e.Value.(*node).b[en+1] == '-') {
								en++
							}
							for en = en + 1; en < len(e.Value.(*node).b); en++ {
								if !isDigit(rune(e.Value.(*node).b[en])) {
									break
								}
							}
						}
						s.extract(st, en, e, token.FLOAT)
						goto numb
					} else {
						// INT
						s.extract(st, en, e, token.INT)
						goto numb
					}
				} else {
					for ; st < len(e.Value.(*node).b); st++ {
						if e.Value.(*node).b[st] != '_' &&
							!isDigit(rune(e.Value.(*node).b[st])) &&
							!isLetter(rune(e.Value.(*node).b[st])) {
							break
						}
					}
					if st >= len(e.Value.(*node).b) {
						break
					}
				}
			}
		}
	}
}

func (s *scanner) scanGoto() {
G:
	for e := s.nodes.Front(); e != nil; e = e.Next() {
		if !(e.Value.(*node).tok == token.IDENT && string(e.Value.(*node).b) == "GO") {
			continue
		}
		n := e.Next()
		if n == nil {
			continue
		}
		if !(n.Value.(*node).tok == token.IDENT && string(n.Value.(*node).b) == "TO") {
			continue
		}
		e.Value.(*node).tok = token.GOTO
		e.Value.(*node).b = []byte("goto")
		s.nodes.Remove(n)
		goto G
	}
}

// isLetter returns true if the rune is a letter.
func isLetter(ch rune) bool { return (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') }

// isDigit returns true if the rune is a digit.
func isDigit(ch rune) bool { return (ch >= '0' && ch <= '9') }
