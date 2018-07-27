package fortran

import (
	"bufio"
	"bytes"
	"fmt"
	"go/format"
	"go/token"
	"io/ioutil"
	"os"
	"strings"
	"testing"

	"github.com/Konstantin8105/f4go/util"
)

func getFortranTestFiles(dir string) (files []string, err error) {
	ents, err := ioutil.ReadDir(dir)
	if err != nil {
		return
	}

	for _, ent := range ents {
		if ent.IsDir() {
			var fs []string
			fs, err = getFortranTestFiles(dir + "/" + ent.Name())
			if err != nil {
				return
			}
			files = append(files, fs...)
			continue
		}
		if !strings.HasSuffix(ent.Name(), ".f") &&
			!strings.HasSuffix(ent.Name(), ".f90") {
			continue
		}
		files = append(files, dir+"/"+ent.Name())
	}

	return
}

func TestScanner(t *testing.T) {

	files, err := getFortranTestFiles("../testdata")
	if err != nil {
		fmt.Println("err = ", err)
		return
	}

	var amountFailTests int

	for _, filename := range files {
		index := strings.LastIndex(filename, "/")
		index = strings.LastIndex(filename[:index], "/")
		testName := filename[index+1:]
		fn := filename[:index+1] + testName

		if !testing.Short() {
			t.Run(fmt.Sprintf("scan/%v", testName), func(t *testing.T) {
				file, err := os.Open(filename)
				if err != nil {
					t.Fatal(err)
					return
				}
				defer file.Close()

				s := NewScanner(bufio.NewReader(file))
				buf := &bytes.Buffer{}
				for {
					tok, lit := s.Scan()
					if tok == token.ILLEGAL {
						t.Fatalf("ILLEGAL literal : %v", lit)
						return
					}
					if tok == token.EOF {
						break
					}
					buf.WriteString(fmt.Sprintf("%-20s\t%v\n", view(tok), lit))
				}

				var out string
				out, err = util.IsDiff(fn+".expected", buf.String())
				if err != nil {
					t.Fatal(err)
				}
				if out != "" {
					t.Fatal(out)
				}
			})

			t.Run(fmt.Sprintf("parse/%v", testName), func(t *testing.T) {
				file, err := os.Open(filename)
				if err != nil {
					t.Fatal(err)
					return
				}
				defer file.Close()

				pr := parser{
					sc: NewScanner(file),
				}

				err = pr.prepare()
				if err != nil {
					t.Fatal(err)
				}
				out, err := util.IsDiff(fn+".f_expect", a(pr.ns))
				if err != nil {
					t.Fatal(err)
				}
				if out != "" {
					t.Fatal(out)
				}
			})
		}

		t.Run(fmt.Sprintf("transpile/%v", testName), func(t *testing.T) {
			file, err := os.Open(filename)
			if err != nil {
				t.Fatal(err)
				return
			}
			defer file.Close()

			pr := parser{
				sc: NewScanner(file),
			}

			err = pr.parse()
			if err != nil {
				amountFailTests++
				t.Fatal(err)
			}

			var buf bytes.Buffer
			if err = format.Node(&buf, token.NewFileSet(), &pr.ast); err != nil {
				t.Fatal(err)
			}

			fmt.Println(buf.String())

			// out, err := util.IsDiff(fn+".go", a(pr.ns))
			// if err != nil {
			// 	t.Fatal(err)
			// }
			// if out != "" {
			// 	t.Fatal(out)
			// }
		})
	}

	t.Logf("Amount fail tests = %v", amountFailTests)
}
