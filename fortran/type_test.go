package fortran

import (
	"go/token"
	"testing"
)

func TestParseType(t *testing.T) {
	tcs := []struct {
		nodes []node
		typ   string
	}{
		{
			nodes: []node{
				{tok: ftInteger, b: []byte("INTEGER")},
			},
			typ: "int",
		},
		{
			nodes: []node{
				{tok: ftCharacter, b: []byte("CHARACTER")},
				{tok: token.MUL, b: []byte("*")},
				{tok: token.LPAREN, b: []byte("(")},
				{tok: token.MUL, b: []byte("*")},
				{tok: token.RPAREN, b: []byte(")")},
			},
			typ: "[]byte",
		},
		{
			nodes: []node{
				{tok: ftCharacter, b: []byte("CHARACTER")},
				{tok: token.MUL, b: []byte("*")},
				{tok: token.INT, b: []byte("32")},
			},
			typ: "[]byte",
		},
		{
			nodes: []node{
				{tok: ftInteger, b: []byte("INTEGER")},
				{tok: token.LPAREN, b: []byte("(")},
				{tok: token.MUL, b: []byte("*")},
				{tok: token.COMMA, b: []byte(",")},
				{tok: token.INT, b: []byte("32")},
				{tok: token.RPAREN, b: []byte(")")},
			},
			typ: "[32][]int",
		},
		{
			nodes: []node{
				{tok: ftDouble, b: []byte("DOUBLE")},
				{tok: ftPrecision, b: []byte("PRECISION")},
				{tok: token.LPAREN, b: []byte("(")},
				{tok: token.MUL, b: []byte("*")},
				{tok: token.RPAREN, b: []byte(")")},
			},
			typ: "[]float64",
		},
		// CHARACTER(1) SRNAME_ARRAY(32)
		{
			nodes: []node{
				{tok: ftCharacter, b: []byte("CHARACTER")},
				{tok: token.LPAREN, b: []byte("(")},
				{tok: token.INT, b: []byte("1")},
				{tok: token.RPAREN, b: []byte(")")},
				{tok: token.LPAREN, b: []byte("(")},
				{tok: token.INT, b: []byte("32")},
				{tok: token.RPAREN, b: []byte(")")},
			},
			typ: "[32]byte",
		},
	}

	for _, tc := range tcs {
		t.Run(nodesToString(tc.nodes), func(t *testing.T) {
			act := parseType(tc.nodes)
			isSame := true
			if act.String() != tc.typ {
				isSame = false
			}
			if !isSame {
				t.Fatalf("types is not same: `%s` != `%s`", act, tc.typ)
			}
		})
	}
}
