package fortran

import (
	"strconv"
	"testing"

	"github.com/bradleyjkemp/cupaloy"
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
			out: []string{"-", "0.004", "-", "S", "-", "123", "-", "12.34e-5", "+", "3e5",
				"-", "9e-5", "+", "2.e22"},
		},
		{
			in:  "      DATA ZERO,ONE,TWO/0.E0,1.E0,2.E0/",
			out: []string{"DATA", "ZERO", ",", "ONE", ",", "TWO", "/", "0.e0", ",", "1.e0", ",", "2.e0", "/"},
		},
		{
			in:  "         SH11 = ZERO",
			out: []string{"SH11", "=", "ZERO"},
		},
		{
			in:  "         GO TO 12",
			out: []string{"goto", "12"},
		},
		{
			in: ` 	          SUBROUTINE real_test()
               END`,
			out: []string{"SUBROUTINE", "real_test", "(", ")", "\n", "END"},
		},
	}
	for i, tc := range tcs {
		t.Run(strconv.Itoa(i), func(t *testing.T) {
			ns := scan([]byte(tc.in))
			if len(ns) != len(tc.out) {
				t.Fatalf("Not same : %v != %v", len(ns), len(tc.out))
			}
			for j := 0; j < len(ns); j++ {
				if tc.out[j] != string(ns[j].b) {
					t.Fatalf("Not same: `%s` != `%s`",
						tc.out[j],
						string(ns[j].b))
				}
			}

			testName := "Scan" + strconv.Itoa(i)
			output := lv(ns)

			if err := cupaloy.SnapshotMulti(testName, output); err != nil {
				t.Fatalf("error: %s", err)
			}
		})
	}
}
