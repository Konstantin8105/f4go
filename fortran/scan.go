package fortran

import (
	"bytes"
	"container/list"
	"fmt"
	"go/token"
)

type ele struct {
	tok token.Token
	b   []rune
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
		b:   bytes.Runes(b),
	})

	// separate lines
	s.scanBreakLines()

	// separate comments
	s.scanComments()

	// separate strings
	s.scanStrings()

	// preprocessor: add specific spaces
	// s.preprocessor()

	// separate on other token
	// s.scanTokens()

	// check ILLEGAL tokens
	// s.checkScan()

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
	}, e)
	s.eles.InsertAfter(&ele{
		tok: undefine,
		b:   aft,
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
