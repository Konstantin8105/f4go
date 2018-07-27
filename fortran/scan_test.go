package fortran

import (
	"container/list"
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
	"testing"
)

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
			in:  "RRR\n sdfse rw\nRRR",
			out: []string{"RRR", "\n", "sdfse", "rw", "\n", "RRR"},
		},
	}
	for i, tc := range tcs {
		t.Run(strconv.Itoa(i), func(t *testing.T) {
			l := scanT([]byte(tc.in))
			lv(l)
			var le int
			for e, j := l.Front(), 0; e != nil; e, j = e.Next(), j+1 {
				if tc.out[j] != string(e.Value.(*ele).b) {
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
			fmt.Printf("%20s\t%10s\t|`%s`\n",
				view(e.Value.(*ele).tok),
				fmt.Sprintf("%v", e.Value.(*ele).pos),
				b)
			// } else {
			// 	fmt.Printf("%20s\n",
			// 		view(e.Value.(*ele).tok))
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
