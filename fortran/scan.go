package fortran

import (
	"bytes"
	"container/list"
	"fmt"
	"go/token"
	"io/ioutil"
	"os"
	"path/filepath"
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
			if !isSpace(b[st]) {
				break
			}
		}

		var end int
		for end = st; end < len(b) && !isSpace(b[end]); end++ {
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

var Debug bool = true // false

func scan(b []byte) (ns []node) {
	Debugf("Begin of scan")

	var s scanner
	s.nodes = list.New()
	s.nodes.PushFront(&node{
		tok: ftUndefine,
		b:   b,
		pos: position{
			line: 1,
			col:  1,
		},
	})
	defer func() {
		for e := s.nodes.Front(); e != nil; e = e.Next() {
			ns = append(ns, *e.Value.(*node))
		}
	}()

	// separate lines
	Debugf("Scan: break lines")
	s.scanBreakLines()

	// separate comments
	Debugf("Scan: comments")
	s.scanComments()

	// merge lines
	Debugf("Scan: merge lines")
	s.mergeLines()

	// separate strings
	Debugf("Scan: strings")
	s.scanStrings()

	// comments !
	Debugf("Scan: comments !")
	s.scanNextComments()

	// preprocessor: add specific spaces
	Debugf("Scan: tokens with point")
	s.scanTokenWithPoint()

	// move comments
	Debugf("Scan: move comments after RPAREN")
	s.scanMoveComment()

	// separate on other token
	Debugf("Scan: tokens")
	s.scanTokens()

	// remove empty
	Debugf("Scan: empty")
	s.scanEmpty()

	// scan numbers
	Debugf("Scan: numbers")
	s.scanNumbers()

	// remove empty
	Debugf("Scan: empty")
	s.scanEmpty()

	Debugf("Scan: after tokens")
	s.scanTokensAfter()

	// remove empty
	Debugf("Scan: empty")
	s.scanEmpty()

	// IDENT for undefine
	Debugf("Scan: undefine idents")
	for e := s.nodes.Front(); e != nil; e = e.Next() {
		switch e.Value.(*node).tok {
		case ftUndefine:
			e.Value.(*node).tok = token.IDENT
			e.Value.(*node).b = bytes.ToUpper(e.Value.(*node).b)
		}
	}

	// token GO TO
	Debugf("Scan: token GOTO")
	s.scanGoto()

	// ::
	Debugf("Scan: token DOUBLE_COLON ::")
	s.scanDoubleColon()

	// postprocessor
	Debugf("Scan: run postprocessor")
	s.postprocessor()

	Debugf("Scan: end of postprocessor")

	// show AST tokens
	//
	// for e := s.nodes.Front(); e != nil; e = e.Next() {
	// 	fmt.Println(e)
	// }
	return
}

// separate break lines
func (s *scanner) scanBreakLines() {
B:
	var again bool
	for e := s.nodes.Front(); e != nil; e = e.Next() {
		if e.Value.(*node).tok != ftUndefine {
			// ignore
			continue
		}
		for j := len(e.Value.(*node).b) - 1; j >= 0; j-- {
			if e.Value.(*node).b[j] != '\n' {
				continue
			}
			s.extract(j, j+1, e, ftNewLine)
			again = true
		}
	}
	if again {
		goto B
	}
	line := 1
	for e := s.nodes.Front(); e != nil; e = e.Next() {
		e.Value.(*node).pos.line = line
		e.Value.(*node).pos.col = 1
		if e.Value.(*node).tok == ftNewLine {
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
		case ftUndefine:
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
}

// comments inside line : '!'
func (s *scanner) scanNextComments() {
Op:
	var again bool
	for e := s.nodes.Front(); e != nil; e = e.Next() {
		if e.Value.(*node).tok != ftUndefine {
			continue
		}
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
	if again {
		goto Op
	}

	for e := s.nodes.Front(); e != nil; e = e.Next() {
		if e.Value.(*node).tok != token.COMMENT {
			continue
		}
		if e.Value.(*node).b[0] != '!' {
			continue
		}
		n := e.Next()
	next:
		if n == nil {
			continue
		}
		if n.Value.(*node).pos.line != e.Value.(*node).pos.line {
			continue
		}
		e.Value.(*node).b = append(e.Value.(*node).b, n.Value.(*node).b...)
		s.nodes.Remove(n)
		n = e.Next()
		goto next
	}

	for e := s.nodes.Front(); e != nil; e = e.Next() {
		if e.Value.(*node).tok != token.COMMENT {
			continue
		}
		if e.Value.(*node).b[0] != '!' {
			continue
		}
		s.nodes.InsertBefore(&node{tok: ftNewLine, b: []byte("\n")}, e)
	}
}

func (s *scanner) mergeLines() {
	if s.nodes.Len() < 2 {
		return
	}
merge:
	for e := s.nodes.Front(); e != nil; e = e.Next() {
		if e.Value.(*node).tok != ftUndefine {
			continue
		}
		if len(e.Value.(*node).b) > 6 && !isSpace(e.Value.(*node).b[5]) {
			p := e.Prev()
			if p == nil {
				continue
			}
			if last := p.Prev(); last != nil && last.Value.(*node).tok == token.COMMENT {
				continue
			}
			if p.Value.(*node).tok != ftNewLine {
				continue
			}
			isEmpty := true
			for i := 0; i < 5; i++ {
				if !isSpace(e.Value.(*node).b[i]) {
					isEmpty = false
				}
			}
			if !isEmpty {
				continue
			}
			s.nodes.Remove(p)
			e.Value.(*node).pos.col += 6
			e.Value.(*node).b = e.Value.(*node).b[6:]
			p = e.Prev()
			if p == nil {
				continue
			}
			p.Value.(*node).b = append(p.Value.(*node).b,
				append([]byte("  "), e.Value.(*node).b...)...)
			s.nodes.Remove(e)
			goto merge
		}
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
		panic(fmt.Errorf("outside of slice {%v,%v} : %v , %v", end, len(b), e.Value.(*node), tok))
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
				tok: ftUndefine,
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
		e.Value.(*node).tok = ftUndefine
		e.Value.(*node).b = bef
		return
	}

	// start is not 0
	// end is not end

	bef, present, aft := b[:start], b[start:end], b[end:]

	e.Value.(*node).tok = ftUndefine
	e.Value.(*node).b = bef

	s.nodes.InsertAfter(&node{
		tok: ftUndefine,
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
again:
	for e := s.nodes.Front(); e != nil; e = e.Next() {
		if e.Value.(*node).tok != ftUndefine {
			continue
		}
		for j := 0; j < len(e.Value.(*node).b); j++ {
			ch := e.Value.(*node).b[j]
			if ch != '"' && ch != '\'' {
				continue
			}
			b := e.Value.(*node).b
			var end int
			for end = j + 1; end < len(b) && b[end] != ch; end++ {
			}
			if end >= len(b) {
				s.extract(j, len(b), e, token.STRING)
			} else {
				s.extract(j, end+1, e, token.STRING)
			}
			j = end + 1
		}
	}
	// merge strings. Example:
	// 9949 FORMAT( 3X, I2, ': norm( L - A * Q'' ) / ( N * norm(A) * EPS )' )
	//                                        == here
	for e := s.nodes.Front(); e != nil; e = e.Next() {
		if e.Value.(*node).tok != token.STRING {
			continue
		}
		n := e.Next()
		if n == nil {
			continue
		}
		if n.Value.(*node).tok != token.STRING {
			continue
		}
		e.Value.(*node).b = append(e.Value.(*node).b[:len(e.Value.(*node).b)-1],
			append([]byte("'"), n.Value.(*node).b[1:]...)...)
		s.nodes.Remove(n)
		goto again
	}
	for e := s.nodes.Front(); e != nil; e = e.Next() {
		if e.Value.(*node).tok != token.STRING {
			continue
		}
		e.Value.(*node).b = bytes.Replace(e.Value.(*node).b, []byte("\""), []byte("'"), -1)
		if e.Value.(*node).b[0] == '\'' {
			e.Value.(*node).b[0] = '"'
		}
		if e.Value.(*node).b[len(e.Value.(*node).b)-1] == '\'' {
			e.Value.(*node).b[len(e.Value.(*node).b)-1] = '"'
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
		{tok: token.LEQ, pattern: "<="},
		{tok: token.GEQ, pattern: ".GE."},
		{tok: token.GEQ, pattern: ">="},
		{tok: token.NOT, pattern: ".NOT."},
		{tok: token.NEQ, pattern: ".NE."},
		{tok: token.NEQ, pattern: ".NEQV."},
		{tok: token.EQL, pattern: ".EQ."},
		{tok: token.EQL, pattern: ".EQV."},
		{tok: token.LAND, pattern: ".AND."},
		{tok: token.LAND, pattern: ".AND ."}, //add for test other/sgelq.f
		{tok: token.LOR, pattern: ".OR."},
		{tok: token.IDENT, pattern: ".TRUE."},
		{tok: token.IDENT, pattern: ".FALSE."},

		// !=
		{tok: token.NEQ, pattern: "/="},
		// other
		{tok: ftDoubleColon, pattern: "::"},
		{tok: token.COLON, pattern: ":"},
		{tok: token.COMMA, pattern: ","},
		{tok: token.LPAREN, pattern: "("},
		{tok: token.RPAREN, pattern: ")"},
		{tok: token.ASSIGN, pattern: "="},
		{tok: token.GTR, pattern: ">"},
		{tok: token.LSS, pattern: "<"},
		{tok: ftDollar, pattern: "$"},
		// stars
		{tok: ftDoubleStar, pattern: "**"},
		{tok: token.MUL, pattern: "*"},
		// devs
		{tok: ftStringConcat, pattern: "//"},
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
		if e.Value.(*node).tok != ftUndefine {
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
	// from:
	// DIMENSION M(100), A(2)
	// to:
	// DIMENSION M(100)
	// DIMENSION A(2)
	for e := s.nodes.Front(); e != nil; e = e.Next() {
		if e.Value.(*node).tok != ftDimension {
			continue
		}
		// TODO
	}

	// From:
	//  END SUBROUTINE
	//  END IF
	// To:
	//  END
	for e := s.nodes.Front(); e != nil; e = e.Next() {
		if e.Value.(*node).tok != ftEnd {
			continue
		}
		// for: END =
		if n := e.Next(); n != nil && n.Value.(*node).tok == token.ASSIGN {
			continue
		}
		for n := e.Next(); n != nil; n = e.Next() {
			if n.Value.(*node).tok != ftNewLine {
				s.nodes.Remove(n)
			} else {
				break
			}
		}
	}

	// From:
	//   ELSEIF
	// To:
	//   ELSE IF
	for e := s.nodes.Front(); e != nil; e = e.Next() {
		if e.Value.(*node).tok == ftElseif {
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

	// Multiline expression
	// if any in column 6, then merge lines
multi:
	for e := s.nodes.Front(); e != nil; e = e.Next() {
		if e.Value.(*node).tok == ftNewLine {
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
			if n.Value.(*node).tok == ftNewLine {
				s.nodes.Remove(n)
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

	// FLOAT correction
	for e := s.nodes.Front(); e != nil; e = e.Next() {
		if e.Value.(*node).tok != token.FLOAT {
			continue
		}
		e.Value.(*node).b = []byte(strings.ToLower(string(e.Value.(*node).b)))
		e.Value.(*node).b = []byte(strings.Replace(string(e.Value.(*node).b), "d", "e", -1))
		e.Value.(*node).b = []byte(strings.Replace(string(e.Value.(*node).b), "q", "e", -1))
	}

	// FROM:
	//	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	//	IMPLICIT INTEGER(I-N)
	//	IMPLICIT COMPLEX (U,V,W), CHARACTER*4 (C,S)
	// TO:
	//	IMPLICIT DOUBLE PRECISION (A)
	//	IMPLICIT DOUBLE PRECISION ...
	//	IMPLICIT DOUBLE PRECISION (H)
	//	IMPLICIT INTEGER (I)
	//	IMPLICIT INTEGER ...
	//	IMPLICIT INTEGER (N)
	//
	// FROM:
	//	IMPLICIT NONE
	// TO:
	// 	nothing
	iter := 0
impl:
	iter++
	if iter > 100000 {
		panic(fmt.Errorf("Too many IMPLICIT iterations"))
	}
	Debugf("finding next IMPLICIT...  %d", iter)

	for e := s.nodes.Front(); e != nil; e = e.Next() {
		if e.Value.(*node).tok != ftImplicit {
			continue
		}

		// split from:
		//	IMPLICIT COMPLEX (U,V,W), CHARACTER*4 (C,S)
		// to:
		//	IMPLICIT COMPLEX (U,V,W)
		//	IMPLICIT CHARACTER*4 (C,S)
		for n := e.Next(); n != nil; n = n.Next() {
			if n.Value.(*node).tok == ftNewLine {
				break
			}
			if n.Value.(*node).tok == token.COMMA && // ,
				n.Prev().Value.(*node).tok == token.RPAREN { // )
				// need split
				n.Value.(*node).tok = ftImplicit
				n.Value.(*node).b = []byte("IMPLICIT")
				s.nodes.InsertBefore(&node{
					tok: ftNewLine,
					b:   []byte("\n"),
				}, n)
				goto impl
			}
		}

		// FROM:
		//	IMPLICIT NONE
		// TO:
		// 	nothing
		{
			n := e.Next()
			if n != nil && n.Value.(*node).tok == token.IDENT &&
				bytes.Equal([]byte("NONE"), bytes.ToUpper(n.Value.(*node).b)) {

				e.Value.(*node).tok = ftNewLine
				e.Value.(*node).b = []byte{'\n'}
				n.Value.(*node).tok = ftNewLine
				n.Value.(*node).b = []byte{'\n'}

				continue
			}
		}

		// split from:
		//	IMPLICIT COMPLEX (U,V,W)
		// to:
		//	IMPLICIT COMPLEX (U)
		//	IMPLICIT COMPLEX (V)
		//	IMPLICIT COMPLEX (W)
		// or
		//	IMPLICIT COMPLEX (A-C)
		// to
		//	IMPLICIT COMPLEX (A,B,C)
		haveRparen := false
		var n *list.Element
		for n = e.Next(); n != nil; n = n.Next() {
			if n.Value.(*node).tok == ftNewLine {
				if n.Prev().Value.(*node).tok == token.RPAREN {
					haveRparen = true
				}
				break
			}
		}
		if !haveRparen {
			continue
		}
		var nodes []*node
		n = n.Prev() // now RPAREN
		withSub := false
		for n = n.Prev(); n != nil; n = n.Prev() {
			if n.Value.(*node).tok == token.COMMA {
				continue
			}
			if n.Value.(*node).tok == token.SUB { // -
				withSub = true
				n.Value.(*node).tok = token.COMMA
				continue
			}
			if n.Value.(*node).tok == token.LPAREN {
				break
			}
			nodes = append(nodes, n.Value.(*node))
		}
		// reverse nodes
		for left, right := 0, len(nodes)-1; left < right; left, right = left+1, right-1 {
			nodes[left], nodes[right] = nodes[right], nodes[left]
		}
		if len(nodes) == 1 {
			continue
		}
		if withSub && len(nodes) == 2 {
			// case :
			//	IMPLICIT COMPLEX (A-C)
			// in nodes : [A  C]
			// transform for
			//	IMPLICIT COMPLEX (A,B,C)
			var names []byte
			for ch := int(nodes[0].b[0]) + 1; ch < int(nodes[1].b[0]); ch++ {
				names = append(names, byte(ch))
			}
			for i := range names {
				s.nodes.InsertAfter(&node{
					tok: token.COMMA,
					b:   []byte{','},
				}, n)
				s.nodes.InsertAfter(&node{
					tok: token.IDENT,
					b:   []byte{names[i]},
				}, n)
			}
			goto impl
		}
		//	IMPLICIT COMPLEX (U,V)
		// to:
		//	IMPLICIT COMPLEX (U)
		//	IMPLICIT COMPLEX (V)

		// get type of variables
		var typ []node
		for n := e.Next(); n != nil; n = n.Next() {
			if n.Value.(*node).tok == token.LPAREN {
				break
			}
			typ = append(typ, *(n.Value.(*node)))
		}
		// generate inject nodes
		var inject []node
		for i := range nodes {
			inject = append(inject,
				node{
					tok: ftImplicit,
					b:   []byte("IMPLICIT"),
				},
			)
			inject = append(inject, typ...)
			inject = append(inject,
				node{
					tok: token.LPAREN,
					b:   []byte{'('},
				},
				node{
					tok: token.IDENT,
					b:   []byte{nodes[i].b[0]},
				},
				node{
					tok: token.RPAREN,
					b:   []byte{')'},
				},
				node{
					tok: ftNewLine,
					b:   []byte{'\n'},
				},
			)
		}
		// inject new code and remove old
		for i := 0; i < len(inject); i++ {
			s.nodes.InsertBefore(&(inject[i]), e)
		}
		var rem []*list.Element
		for n := e; n != nil && n.Value.(*node).tok != ftNewLine; n = n.Next() {
			rem = append(rem, n)
		}
		for i := range rem {
			s.nodes.Remove(rem[i])
		}

		goto impl
	}

	// inject code from INCLUDE
incl:
	for e := s.nodes.Front(); e != nil; e = e.Next() {
		if e.Value.(*node).tok != ftInclude {
			continue
		}
		e.Value.(*node).tok = ftNewLine
		e.Value.(*node).b = []byte{'\n'}
		// Example :
		// include 'file'
		var filename []byte
		for n := e.Next(); n != nil && n.Value.(*node).tok != ftNewLine; n = n.Next() {
			filename = append(filename, n.Value.(*node).b...)
			n.Value.(*node).tok = ftNewLine
			n.Value.(*node).b = []byte{'\n'}
		}

		for _, sep := range []byte{'\'', '"'} {
			if filename[0] == sep {
				filename = filename[1:]
			}
			if filename[len(filename)-1] == sep {
				filename = filename[:len(filename)-1]
			}
		}

		var dat []byte

		if err := filepath.Walk(".", func(path string, info os.FileInfo, err error) error {
			if err != nil {
				return nil
			}

			if info.Name() == string(filename) {
				var ef error
				dat, ef = ioutil.ReadFile(path)
				return ef
			}
			return nil
		}); err != nil {
			continue
		}

		ns := scan(dat)
		for i := range ns {
			s.nodes.InsertBefore(&ns[i], e)
		}

		goto incl
	}

}

func (s *scanner) scanMoveComment() {
	// check amount PAREN
	var left, rigth int
	for e := s.nodes.Front(); e != nil; e = e.Next() {
		switch tok := e.Value.(*node).tok; tok {
		case token.LPAREN:
			left++
		case token.RPAREN:
			rigth++
		}
	}
	if left != rigth {
		Debugf("Amount of left and rigth paren is not same: %d != %d", left, rigth)

		return
	}
	Debugf("Amount of left and rigth paren is same: %d", left)
}

func (s *scanner) scanTokens() {
	entities := []struct {
		tok     token.Token
		pattern []string
	}{
		{tok: ftSubroutine, pattern: []string{"SUBROUTINE"}},
		{tok: ftImplicit, pattern: []string{"IMPLICIT"}},
		{tok: ftInteger, pattern: []string{"INTEGER"}},
		{tok: ftCharacter, pattern: []string{"CHARACTER"}},
		{tok: ftLogical, pattern: []string{"LOGICAL"}},
		{tok: ftComplex, pattern: []string{"COMPLEX"}},
		{tok: ftReal, pattern: []string{"REAL"}},
		{tok: ftData, pattern: []string{"DATA"}},
		{tok: ftExternal, pattern: []string{"EXTERNAL"}},
		{tok: ftEnd, pattern: []string{"END", "ENDDO", "ENDIF"}},
		{tok: ftDo, pattern: []string{"DO"}},
		{tok: ftDouble, pattern: []string{"DOUBLE"}},
		{tok: ftDimension, pattern: []string{"DIMENSION"}},
		{tok: ftFunction, pattern: []string{"FUNCTION"}},
		{tok: token.IF, pattern: []string{"IF"}},
		{tok: token.ELSE, pattern: []string{"ELSE"}},
		{tok: token.CONTINUE, pattern: []string{"CONTINUE"}},
		{tok: ftCall, pattern: []string{"CALL"}},
		{tok: ftThen, pattern: []string{"THEN"}},
		{tok: token.RETURN, pattern: []string{"RETURN"}},
		{tok: ftWrite, pattern: []string{"WRITE"}},
		{tok: ftPrint, pattern: []string{"PRINT"}},
		{tok: ftWhile, pattern: []string{"WHILE"}},
		{tok: ftParameter, pattern: []string{"PARAMETER"}},
		{tok: ftProgram, pattern: []string{"PROGRAM"}},
		{tok: ftPrecision, pattern: []string{"PRECISION"}},
		{tok: ftIntrinsic, pattern: []string{"INTRINSIC"}},
		{tok: ftFormat, pattern: []string{"FORMAT"}},
		{tok: ftStop, pattern: []string{"STOP"}},
		{tok: token.GOTO, pattern: []string{"GOTO"}},
		{tok: ftElseif, pattern: []string{"ELSEIF"}},
		{tok: ftSave, pattern: []string{"SAVE"}},
		{tok: ftOpen, pattern: []string{"OPEN"}},
		{tok: ftRead, pattern: []string{"READ"}},
		{tok: ftAssign, pattern: []string{"ASSIGN"}},
		{tok: ftClose, pattern: []string{"CLOSE"}},
		{tok: ftDefine, pattern: []string{"#DEFINE"}},
		{tok: ftEquivalence, pattern: []string{"EQUIVALENCE"}},
		{tok: ftCommon, pattern: []string{"COMMON"}},
		{tok: ftRewind, pattern: []string{"REWIND"}},
		{tok: ftInclude, pattern: []string{"INCLUDE"}},
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
			if e.Value.(*node).tok != ftUndefine {
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
						!(isLetter(e.Value.(*node).b[len(pat)]) ||
							isDigit(e.Value.(*node).b[len(pat)]) ||
							e.Value.(*node).b[len(pat)] == '_') {
						found = true
					}
				}
				if index > 0 {
					if isSpace(e.Value.(*node).b[index-1]) &&
						(len(e.Value.(*node).b) == index+len(pat) ||
							!(isLetter(e.Value.(*node).b[index+len(pat)]) ||
								isDigit(e.Value.(*node).b[index+len(pat)]) ||
								e.Value.(*node).b[index+len(pat)] == '_')) {
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
				case ftUndefine:
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
		if e.Value.(*node).tok != ftUndefine {
			continue
		}
		if len(e.Value.(*node).b) == 0 {
			n := e.Next()
			s.nodes.Remove(e)
			e = n
			again = true
			if n == nil {
				goto empty
			}
			continue
		}
		if len(bytes.TrimSpace([]byte(string(e.Value.(*node).b)))) == 0 {
			n := e.Next()
			s.nodes.Remove(e)
			e = n
			if n == nil {
				goto empty
			}
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
	if again {
		goto empty
	}
}

func (s *scanner) scanNumbers() {
numb:
	var again bool
	for e := s.nodes.Front(); e != nil; e = e.Next() {
		if e.Value.(*node).tok != ftUndefine {
			continue
		}
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
			if isDigit(e.Value.(*node).b[st]) || e.Value.(*node).b[st] == '.' {
				var en int
				for en = st; en < len(e.Value.(*node).b); en++ {
					if !isDigit(e.Value.(*node).b[en]) {
						break
					}
				}
				if en < len(e.Value.(*node).b) &&
					(e.Value.(*node).b[en] == '.' || isFloatLetter(e.Value.(*node).b[en])) {
					// FLOAT
					if e.Value.(*node).b[en] == '.' {
						for en = en + 1; en < len(e.Value.(*node).b); en++ {
							if !isDigit(e.Value.(*node).b[en]) {
								break
							}
						}
					}
					if en < len(e.Value.(*node).b) &&
						(isFloatLetter(e.Value.(*node).b[en])) {
						if en+1 < len(e.Value.(*node).b) &&
							(e.Value.(*node).b[en+1] == '+' || e.Value.(*node).b[en+1] == '-') {
							en++
						}
						for en = en + 1; en < len(e.Value.(*node).b); en++ {
							if !isDigit(e.Value.(*node).b[en]) {
								break
							}
						}
					}
					s.extract(st, en, e, token.FLOAT)
					again = true
					break
				} else {
					// INT
					s.extract(st, en, e, token.INT)
					again = true
					break
				}

				continue
			}

			for ; st < len(e.Value.(*node).b); st++ {
				if e.Value.(*node).b[st] != '_' &&
					!isDigit(e.Value.(*node).b[st]) &&
					!isLetter(e.Value.(*node).b[st]) {
					break
				}
			}
			if st >= len(e.Value.(*node).b) {
				break
			}
		}
	}
	if again {
		Debugf("rescan numbers")
		goto numb
	}
}

// [CHARACTER, `character`, {838 13}]}
// [(, `(`, {838 23}]}
// [IDENT, `LEN`, {838 24}]}
// [=, `=`, {838 27}]}
// [INT, `5`, {838 28}]}
// [), `)`, {838 29}]}
// [DOUBLE_COLON, `::`, {838 31}]}
// [IDENT, `LINE1`, {838 34}]}
//
// [CHARACTER, `character`, {838 13}]}
// [(, `(`, {838 23}]}
// [IDENT, `LEN`, {838 24}]}
// [=, `=`, {838 27}]}
// [INT, `5`, {838 28}]}
// [), `)`, {838 29}]}
// [DOUBLE_COLON, `::`, {838 31}]}
// [IDENT, `LINE1`, {838 34}]}
// [,, `,`, {838 40}]}
// [IDENT, `LINE2`, {838 41}]}
//
// from:
//
//	character (len=5) :: line1 ,line2
//
// to:
//
//	character (len=5) :: line1
//	character (len=5) :: line2
func (s *scanner) scanDoubleColon() {
	for e := s.nodes.Front(); e != nil; e = e.Next() {
		if !(e.Value.(*node).tok == ftDoubleColon) {
			continue
		}
		// go to begin of fortran line and store the elements
		first := list.New()
		so := e
		p := e
		for ; p != nil; p = p.Prev() {
			if p.Value.(*node).tok == ftNewLine {
				break
			}
			c := *(p.Value.(*node))
			first.PushFront(&c)
		}
		begin := p
		// go to end of fortran line and store the indents
		var indents [][]node
		e = e.Next()
		for p = e; p != nil; p = p.Next() {
			if p.Value.(*node).tok == ftNewLine {
				break
			}
			if len(indents) == 0 {
				indents = make([][]node, 1)
				indents[0] = append(indents[0], *(p.Value.(*node)))
				continue
			}
			if p.Value.(*node).tok == token.COMMA {
				indents = append(indents, make([]node, 0))
				continue
			}
			l := len(indents) - 1
			indents[l] = append(indents[l], *(p.Value.(*node)))
		}
		// replace to newline and inject new
		for p := begin.Next(); p != nil; p = p.Next() {
			if p.Value.(*node).tok == ftNewLine {
				break
			}
			p.Value.(*node).tok = ftNewLine
		}
		for i := range indents {
			for e := first.Front(); e != nil; e = e.Next() {
				c := *(e.Value.(*node))
				s.nodes.InsertBefore(&c, so)
			}
			for j := range indents[i] {
				c := indents[i][j]
				s.nodes.InsertBefore(&c, so)
			}
			s.nodes.InsertBefore(&node{tok: ftNewLine, b: []byte("\n")}, so)
		}
	}
}

func (s *scanner) scanGoto() {
G:
	for e := s.nodes.Front(); e != nil; e = e.Next() {
		if !(e.Value.(*node).tok == token.IDENT &&
			strings.ToUpper(string(e.Value.(*node).b)) == "GO") {
			continue
		}
		n := e.Next()
		if n == nil {
			continue
		}
		if !(n.Value.(*node).tok == token.IDENT &&
			strings.ToUpper(string(n.Value.(*node).b)) == "TO") {
			continue
		}
		e.Value.(*node).tok = token.GOTO
		e.Value.(*node).b = []byte("goto")
		s.nodes.Remove(n)
		goto G
	}
}

func isSpace(ch byte) bool { return ch == ' ' || ch == '\t' || ch == '\r' }

// isLetter returns true if the rune is a letter.
func isLetter(ch byte) bool { return (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') }

// isDigit returns true if the rune is a digit.
func isDigit(ch byte) bool { return (ch >= '0' && ch <= '9') }

// isFloatLetter return true if letter used in floats
func isFloatLetter(ch byte) bool {
	return ch == 'E' || ch == 'e' ||
		ch == 'D' || ch == 'd' ||
		ch == 'Q' || ch == 'q'
}
