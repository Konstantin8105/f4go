package fortran

import (
	"container/list"
	"fmt"
	"strconv"
	"testing"
)

func TestEle(t *testing.T) {
	e := &node{
		b: []byte(string("1  4  7 9 1")),
	}
	e.Split()
}

func TestScanIn(t *testing.T) {
	tcs := []struct {
		in  string
		out []string
	}{
		{
			in:  "C ttt",
			out: []string{"C ttt"},
		},
		{
			in:  "\n",
			out: []string{"\n"},
		},
		{
			in:  "\n\n\n\n",
			out: []string{"\n", "\n", "\n", "\n"},
		},
		{
			in:  "C ttt\n  ddd",
			out: []string{"C ttt", "\n", "ddd"},
		},
		{
			in: "C ttt\n  ddd\nC ttt\n  ddd\nC ttt\n  ddd\nC ttt\n  ddd\n",
			out: []string{"C ttt", "\n", "ddd", "\n",
				"C ttt", "\n", "ddd", "\n",
				"C ttt", "\n", "ddd", "\n",
				"C ttt", "\n", "ddd", "\n",
			},
		},
		{
			in:  "RRR\nC sdfse rw\nRRR",
			out: []string{"RRR", "\n", "C sdfse rw", "\n", "RRR"},
		},
		{
			in:  "       sdfse   S  rw   ",
			out: []string{"sdfse", "S", "rw"},
		},
		{
			in:  "RRR\n       sdfse   S  rw   \n E      \nRRR",
			out: []string{"RRR", "\n", "sdfse", "S", "rw", "\n", "E", "\n", "RRR"},
		},
		{
			in: "          -0.004-S-123-12.34Q-5+3E5-9E-5+2.q22",
			out: []string{"-", "0.004", "-", "S", "-", "123", "-", "12.34Q-5", "+", "3E5",
				"-", "9E-5", "+", "2.q22"},
		},
		{
			in:  "      DATA ZERO,ONE,TWO/0.E0,1.E0,2.E0/",
			out: []string{"DATA", "ZERO", ",", "ONE", ",", "TWO", "/", "0.E0", ",", "1.E0", ",", "2.E0", "/"},
		},
		{
			in:  "         SH11 = ZERO",
			out: []string{"SH11", "=", "ZERO"},
		},
		{
			in:  "         GO TO 12",
			out: []string{"goto", "12"},
		},
	}
	for i, tc := range tcs {
		t.Run(strconv.Itoa(i), func(t *testing.T) {
			l := scan([]byte(tc.in))
			// lv(l)// Only for debuging
			var le int
			for e, j := l.Front(), 0; e != nil; e, j = e.Next(), j+1 {
				if j < len(tc.out) && tc.out[j] != string(e.Value.(*node).b) {
					t.Fatalf("Not same: `%s` != `%s`",
						tc.out[j],
						string(e.Value.(*node).b))
				}
				le++
			}
			if le != len(tc.out) {
				t.Fatalf("Not same : %v != %v", le, len(tc.out))
			}
		})
	}
}

func lv(l *list.List) {
	for e := l.Front(); e != nil; e = e.Next() {
		b := string(e.Value.(*node).b)
		if e.Value.(*node).tok != NEW_LINE {
			fmt.Printf("%10s\t%10s\t|`%s`\n",
				view(e.Value.(*node).tok),
				fmt.Sprintf("%v", e.Value.(*node).pos),
				b)
		} else {
			fmt.Printf("%20s\n",
				view(e.Value.(*node).tok))
		}
	}
}
