package fortran

import (
	"bytes"
	"fmt"
	"go/format"
	"go/token"
	"io/ioutil"
	"strings"
	"testing"
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
		// fn := filename[:index+1] + testName
		//
		// if !testing.Short() {
		// 	t.Run(fmt.Sprintf("scan/%v", testName), func(t *testing.T) {
		//
		// 		// read body of file
		// 		b, err := ioutil.ReadFile(filename)
		// 		if err != nil {
		// 			t.Fatal(err)
		// 		}
		//
		// 		s := newScanner(b)
		// 		buf := &bytes.Buffer{}
		// 		for {
		// 			tok, lit := s.scan()
		// 			if tok == token.ILLEGAL {
		// 				t.Fatalf("ILLEGAL literal : %v", lit)
		// 				return
		// 			}
		// 			if tok == token.EOF {
		// 				break
		// 			}
		// 			buf.WriteString(fmt.Sprintf("%-20s\t%v\n", view(tok), lit))
		// 		}
		//
		// 		var out string
		// 		out, err = util.IsDiff(fn+".expected", buf.String())
		// 		if err != nil {
		// 			t.Fatal(err)
		// 		}
		// 		if out != "" {
		// 			t.Fatal(out)
		// 		}
		// 	})
		//
		// 	t.Run(fmt.Sprintf("parse/%v", testName), func(t *testing.T) {
		//
		// 		// read body of file
		// 		b, err := ioutil.ReadFile(filename)
		// 		if err != nil {
		// 			t.Fatal(err)
		// 		}
		//
		// 		pr := parser{
		// 			sc: newScanner(b),
		// 		}
		//
		// 		pr.prepare()
		//
		// 		out, err := util.IsDiff(fn+".f_expect", a(pr.ns))
		// 		if err != nil {
		// 			t.Fatal(err)
		// 		}
		// 		if out != "" {
		// 			t.Fatal(out)
		// 		}
		// 	})
		// }

		t.Run(fmt.Sprintf("transpile/%v", testName), func(t *testing.T) {

			// read body of file
			b, err := ioutil.ReadFile(filename)
			if err != nil {
				t.Fatal(err)
			}

			// pr := parser{
			// 	sc: newScanner(b),
			// }

			ast, errs := parse(b)
			if len(errs) > 0 {
				amountFailTests += len(errs)
				for _, err := range errs {
					t.Logf("Error: %v", err)
				}
				t.Fatal("Errors is more zero")
			}

			var buf bytes.Buffer
			if err = format.Node(&buf, token.NewFileSet(), &ast); err != nil {
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

func BenchmarkCgemm(b *testing.B) {
	for i := 0; i < b.N; i++ {
		b.StopTimer()
		// read body of file
		d, err := ioutil.ReadFile("../testdata/blas/cgemm.f")
		if err != nil {
			b.Fatal(err)
		}

		b.StartTimer()
		_, _ = parse(d)
	}
}
