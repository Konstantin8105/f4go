package fortran

import (
	"container/list"
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
	"testing"
)

func TestEle(t *testing.T) {
	e := &ele{
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
			out: []string{"GOTO", "12"},
		},
		{
			in: `      SMAX = ZERO
      DO 20 I = 1, N
         SMAX = I
   20 CONTINUE`,
			out: []string{"SMAX", "=", "ZERO", "\n",
				"DO", "I", "=", "1", ",", "N", "\n",
				"SMAX", "=", "I", "\n",
				"END", "\n"},
		},
	}
	for i, tc := range tcs {
		t.Run(strconv.Itoa(i), func(t *testing.T) {
			l := scanT([]byte(tc.in))
			lv(l)
			var le int
			for e, j := l.Front(), 0; e != nil; e, j = e.Next(), j+1 {
				if j < len(tc.out) && tc.out[j] != string(e.Value.(*ele).b) {
					t.Fatalf("Not same: `%s` != `%s`",
						tc.out[j],
						string(e.Value.(*ele).b))
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
		b := string(e.Value.(*ele).b)
		if e.Value.(*ele).tok != NEW_LINE {
			fmt.Printf("%10s\t%10s\t|`%s`\n",
				view(e.Value.(*ele).tok),
				fmt.Sprintf("%v", e.Value.(*ele).pos),
				b)
		} else {
			fmt.Printf("%20s\n",
				view(e.Value.(*ele).tok))
		}
	}
}

func TestScanR(t *testing.T) {

	files, err := getFortranTestFiles("../testdata")
	if err != nil {
		fmt.Println("err = ", err)
		return
	}

	for _, filename := range files {
		index := strings.LastIndex(filename, "/")
		index = strings.LastIndex(filename[:index], "/")
		testName := filename[index+1:]
		t.Run(fmt.Sprintf("scan/%v", testName), func(t *testing.T) {

			// read body of file
			b, err := ioutil.ReadFile(filename)
			if err != nil {
				t.Fatal(err)
			}
			l := scanT(b)
			lv(l)
		})
	}
}
