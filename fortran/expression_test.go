package fortran

import "testing"

func TestExpressionFail(t *testing.T) {
	var p parser
	p.ns = []node{
		{tok: ftNewLine, b: []byte("\n")},
	}
	_ = p.parseExpr(0, 1)
	if len(p.errs) == 0 {
		t.Fatal("Break line is error for exression parse")
	}
}
