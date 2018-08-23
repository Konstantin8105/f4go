package fortran

import (
	"go/token"
	"strings"
	"testing"
)

func TestSplit(t *testing.T) {
	tcs := []struct {
		in                                                 string
		pos                                                int
		leftOther, leftVariable, rightVariable, rightOther string
	}{
		{
			in:            "t(2**3)",
			pos:           3,
			leftOther:     "T(",
			leftVariable:  "2",
			rightVariable: "3",
			rightOther:    ")",
		},
		{
			in:            "t(replace,2**3)",
			pos:           5,
			leftOther:     "T(REPLACE,",
			leftVariable:  "2",
			rightVariable: "3",
			rightOther:    ")",
		},
		{
			in:            "t(2**3,replace)",
			pos:           3,
			leftOther:     "T(",
			leftVariable:  "2",
			rightVariable: "3",
			rightOther:    ",REPLACE)",
		},
		{
			in:            "t(a(:1) // b(:1))",
			pos:           7,
			leftOther:     "T(",
			leftVariable:  "A(:1)",
			rightVariable: "B(:1)",
			rightOther:    ")",
		},
	}

	for _, tc := range tcs {
		t.Run(tc.in, func(t *testing.T) {
			var p parser
			p.initVars.add("a", goType{baseType: "byte"})
			p.initVars.add("b", goType{baseType: "byte"})

			nodes := scan([]byte(tc.in))
			leftOther, leftVariable, rightVariable, rightOther :=
				p.split(&nodes, tc.pos)

			compare := []struct {
				name   string
				expect string
				actual string
			}{
				{
					name:   "leftOther",
					expect: strings.Replace(tc.leftOther, " ", "", -1),
					actual: strings.Replace(nodesToString(leftOther), " ", "", -1),
				},
				{
					name:   "leftVariable",
					expect: strings.Replace(tc.leftVariable, " ", "", -1),
					actual: strings.Replace(nodesToString(leftVariable), " ", "", -1),
				},
				{
					name:   "rightVariable",
					expect: strings.Replace(tc.rightVariable, " ", "", -1),
					actual: strings.Replace(nodesToString(rightVariable), " ", "", -1),
				},
				{
					name:   "rightOther",
					expect: strings.Replace(tc.rightOther, " ", "", -1),
					actual: strings.Replace(nodesToString(rightOther), " ", "", -1),
				},
			}

			for _, c := range compare {
				t.Run(c.name, func(t *testing.T) {
					if c.expect != c.actual {
						t.Fatalf("%s not same: `%s` != `%s`",
							c.name, c.expect, c.actual)
					}
				})
			}
		})
	}
}

func TestSeparateArgsParenFail(t *testing.T) {
	defer func() {
		if r := recover(); r == nil {
			t.Fatal("test is not fail")
		}
	}()
	separateArgsParen([]node{{tok: token.INT, b: []byte("42")}})
}

func TestSeparateArgsParen(t *testing.T) {
	n := scan([]byte("(real(I),imag(I))"))
	args, _ := separateArgsParen(n)
	if len(args) != 2 {
		t.Fatalf("Not correct arg : %v", args)
	}
	if nodesToString(args[0]) != "real ( I )" {
		t.Fatalf("Not correct arg2 : %v\n%v", args[0], nodesToString(args[0]))
	}
}
