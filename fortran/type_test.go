package fortran

import (
	"go/token"
	"testing"
)

func TestParseType(t *testing.T) {
	tcs := []struct {
		nodes []node
		typ   goType
	}{
		{
			nodes: []node{
				{tok: ftInteger, b: []byte("INTEGER")},
			},
			typ: goType{
				baseType: "int",
			},
		},
		{
			nodes: []node{
				{tok: ftCharacter, b: []byte("CHARACTER")},
				{tok: token.MUL, b: []byte("*")},
				{tok: token.LPAREN, b: []byte("(")},
				{tok: token.MUL, b: []byte("*")},
				{tok: token.RPAREN, b: []byte(")")},
			},
			typ: goType{
				baseType:  "byte",
				arrayType: []int{-1},
			},
		},
		{
			nodes: []node{
				{tok: ftCharacter, b: []byte("CHARACTER")},
				{tok: token.MUL, b: []byte("*")},
				{tok: token.INT, b: []byte("32")},
			},
			typ: goType{
				baseType: "string",
			},
		},
	}

	for _, tc := range tcs {
		t.Run(nodesToString(tc.nodes), func(t *testing.T) {
			act := parseType(tc.nodes)
			isSame := true
			if act.baseType == tc.typ.baseType {
				if len(act.arrayType) == len(tc.typ.arrayType) {
					for i := range act.arrayType {
						if act.arrayType[i] != tc.typ.arrayType[i] {
							isSame = false
						}
					}
				} else {
					isSame = false
				}
			} else {
				isSame = false
			}
			if !isSame {
				t.Fatalf("Basetype is not same: `%s` != `%s`", act, tc.typ)
			}
		})
	}
}
