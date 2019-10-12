package fortran

import (
	"bytes"
	"container/list"
	"fmt"
	"go/token"
	"os"
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

const Debug bool = true // false

func scan(b []byte) (ns []node) {
	if Debug {
		fmt.Fprintf(os.Stdout, "Begin of scan\n")
	}
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
		fmt.Println(ns)
	}()

	// separate lines
	if Debug {
		fmt.Fprintf(os.Stdout, "Scan: break lines\n")
	}
	s.scanBreakLines()

	// separate comments
	if Debug {
		fmt.Fprintf(os.Stdout, "Scan: comments\n")
	}
	s.scanComments()

	// merge lines
	if Debug {
		fmt.Fprintf(os.Stdout, "Scan: merge lines\n")
	}
	s.mergeLines()

	// separate strings
	if Debug {
		fmt.Fprintf(os.Stdout, "Scan: strings\n")
	}
	s.scanStrings()

	// comments !
	if Debug {
		fmt.Fprintf(os.Stdout, "Scan: comments !\n")
	}
	s.scanNextComments()

	// preprocessor: add specific spaces
	if Debug {
		fmt.Fprintf(os.Stdout, "Scan: tokens with point\n")
	}
	s.scanTokenWithPoint()

	// move comments
	if Debug {
		fmt.Fprintf(os.Stdout, "Scan: move comments after RPAREN\n")
	}
	s.scanMoveComment()

	// separate on other token
	if Debug {
		fmt.Fprintf(os.Stdout, "Scan: tokens\n")
	}
	s.scanTokens()

	// remove empty
	if Debug {
		fmt.Fprintf(os.Stdout, "Scan: empty\n")
	}
	s.scanEmpty()

	// scan numbers
	if Debug {
		fmt.Fprintf(os.Stdout, "Scan: numbers\n")
	}
	s.scanNumbers()

	// remove empty
	if Debug {
		fmt.Fprintf(os.Stdout, "Scan: empty\n")
	}
	s.scanEmpty()

	if Debug {
		fmt.Fprintf(os.Stdout, "Scan: after tokens\n")
	}
	s.scanTokensAfter()

	// remove empty
	if Debug {
		fmt.Fprintf(os.Stdout, "Scan: empty\n")
	}
	s.scanEmpty()

	// IDENT for undefine
	if Debug {
		fmt.Fprintf(os.Stdout, "Scan: undefine idents\n")
	}
	for e := s.nodes.Front(); e != nil; e = e.Next() {
		switch e.Value.(*node).tok {
		case ftUndefine:
			e.Value.(*node).tok = token.IDENT
			e.Value.(*node).b = bytes.ToUpper(e.Value.(*node).b)
		}
	}

	// token GO TO
	if Debug {
		fmt.Fprintf(os.Stdout, "Scan: token GOTO\n")
	}
	s.scanGoto()

	// postprocessor
	if Debug {
		fmt.Fprintf(os.Stdout, "Scan: run postprocessor\n")
	}
	s.postprocessor()
	if Debug {
		fmt.Fprintf(os.Stdout, "Scan: end of postprocessor\n")
	}

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
	// DIMENSION M(100)
	// to:
	// INTEGER M(100)
	for e := s.nodes.Front(); e != nil; e = e.Next() {
		if e.Value.(*node).tok != ftDimension {
			continue
		}
		e.Value.(*node).tok = ftInteger
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
	iter := 0
impl:
	iter++
	if iter > 100000 {
		panic(fmt.Errorf("Too many IMPLICIT iterations"))
	}
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
		for n := e.Next(); n != nil && n.Value.(*node).tok != ftNewLine; {
			s.nodes.Remove(n.Prev())
		}

		goto impl
	}

	// from:
	//    COMMON/PDAT/LOC(3), T(1)
	// to:
	//    INTEGER LOC(3)
	//    INTEGER Common.PDAT.LOC(3)
	//    INTEGER T(1)
	//    INTEGER Common.PDAT.T (1)
	//    COMMON/PDAT/LOC(3), T(1)
	// or:
	// from:
	//    COMMON LOC(3), T(1)
	// to:
	//    INTEGER LOC(3)
	//    INTEGER T(1)
	//    COMMON LOC(3), T(1)
	for e := s.nodes.Front(); e != nil; e = e.Next() {
		if e.Value.(*node).tok != ftCommon {
			continue
		}
		n := e.Next()

		var blockName string

		if n.Value.(*node).tok == token.QUO { // /
			n = n.Next()
			for ; n != nil && n.Value.(*node).tok != token.QUO; n = n.Next() {
				blockName = string(n.Value.(*node).b)
			}
			n = n.Next()
		}

		if blockName == "" || blockName == "*" {
			blockName = "ALL"
		}

		isFirst := true
		var names []node
		inject := func() {
			iF := isFirst
			// next nodes are names
			if isFirst {
				s.nodes.InsertBefore(&node{
					tok: ftInteger,
				}, e)
				isFirst = false
			} else {
				s.nodes.InsertBefore(&node{
					tok: ftReal,
				}, e)
			}
			for i := 0; i < len(names); i++ {
				s.nodes.InsertBefore(&names[i], e)
			}
			s.nodes.InsertBefore(&node{tok: ftNewLine, b: []byte{'\n'}}, e)

			// initialize common type
			isFirst = iF
			if isFirst {
				s.nodes.InsertBefore(&node{
					tok: ftInteger,
				}, e)
				isFirst = false
			} else {
				s.nodes.InsertBefore(&node{
					tok: ftReal,
				}, e)
			}
			for i := 0; i < len(names); i++ {
				name := names[i]
				if i == 0 {
					name.b = append([]byte("COMMON."+blockName+"."), name.b...)
				}
				s.nodes.InsertBefore(&name, e)
			}
			s.nodes.InsertBefore(&node{tok: ftNewLine, b: []byte{'\n'}}, e)
		}
		isopen := false
		for ; n != nil; n = n.Next() {
			if n.Value.(*node).tok == ftNewLine {
				inject()
				break
			}
			if n.Value.(*node).tok == token.LPAREN {
				isopen = true
			}
			if n.Value.(*node).tok == token.RPAREN {
				isopen = false
			}
			if n.Value.(*node).tok == token.COMMA && isopen == false {
				inject()
				names = make([]node, 0)
				continue
			}
			names = append(names, *n.Value.(*node))
		}
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
		if Debug {
			fmt.Fprintf(os.Stdout, "Amount of left and rigth paren is not same: %d != %d\n", left, rigth)
		}
		return
	}
	if Debug {
		fmt.Fprintf(os.Stdout, "Amount of left and rigth paren is same: %d\n", left)
	}
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
		{tok: ftEnd, pattern: []string{"END", "ENDDO"}},
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
		if Debug {
			fmt.Fprintf(os.Stdout, "rescan numbers\n")
		}
		goto numb
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
