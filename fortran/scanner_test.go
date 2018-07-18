package fortran

import (
	"bufio"
	"bytes"
	"fmt"
	"go/token"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/Konstantin8105/f4go/util"
)

func getFortranTestFiles() (files []string, err error) {
	locations := []string{
		"../testdata/other/*.f",
		"../testdata/other/*.f90",
		"../testdata/ftnchek/*.f",
		"../testdata/ftnchek/*.f90",
		"../testdata/blas/*.f",
	}
	for _, loc := range locations {
		var fs []string
		fs, err = filepath.Glob(loc)
		if err != nil {
			fmt.Println("err = ", err)
			return
		}
		files = append(files, fs...)
	}
	return
}

func TestScanner(t *testing.T) {

	files, err := getFortranTestFiles()
	if err != nil {
		fmt.Println("err = ", err)
		return
	}

	for _, filename := range files {
		index := strings.LastIndex(filename, "/")
		testName := filename[index+1:]
		t.Run(testName, func(t *testing.T) {
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

			fileName := filename[:index+1] + testName + ".expected"

			var out string
			out, err = util.IsDiff(fileName, buf.String())
			if err != nil {
				t.Fatal(err)
			}
			if out != "" {
				t.Fatal(out)
			}
		})
	}
}
