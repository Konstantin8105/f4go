package fortran

import (
	"bufio"
	"bytes"
	"fmt"
	"go/token"
	"io/ioutil"
	"math"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
)

func getFortranTestFiles() (files []string, err error) {
	locations := []string{
		"../testdata/*.f",
		"../testdata/*.f90",
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
		testName := strings.Replace(filename, "..", "", -1)
		testName = strings.Replace(testName, "/", "_", -1)
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
				buf.WriteString(fmt.Sprintf("%v\t%v\n", tok, lit))
			}

			fileName := "../testdata/expect/" + testName + ".expected"
			if _, err := os.Stat(fileName); err == nil {
				var expect []byte
				expect, err = ioutil.ReadFile(fileName)
				if err != nil {
					t.Fatalf("error: %s", err)
				}

				diff := ShowDiff(buf.String(), string(expect))
				if diff != "" {
					t.Fatalf("%s", diff)
				}
			} else {
				err = ioutil.WriteFile(fileName, buf.Bytes(), 0644)
				if err != nil {
					t.Fatalf("error: %s", err)
				}
			}

			// Update tests
			// UPDATE_SNAPSHOTS=true go test ./fortran/...
			// err = cupaloy.SnapshotMulti(testName, buf.String())
			// if err != nil {
			// 	t.Fatalf("error: %s", err)
			// }
		})
	}
}

// ShowDiff will print two strings vertically next to each other so that line
// differences are easier to read.
func ShowDiff(a, b string) (out string) {
	aLines := strings.Split(a, "\n")
	bLines := strings.Split(b, "\n")
	maxLines := int(math.Max(float64(len(aLines)), float64(len(bLines))))

	for lineNumber := 0; lineNumber < maxLines; lineNumber++ {
		aLine := ""
		bLine := ""

		// Replace NULL characters with a dot. Otherwise the strings will look
		// exactly the same but have different length (and therfore not be
		// equal).
		if lineNumber < len(aLines) {
			aLine = strconv.Quote(aLines[lineNumber])
		}
		if lineNumber < len(bLines) {
			bLine = strconv.Quote(bLines[lineNumber])
		}

		diffFlag := " "
		if aLine != bLine {
			diffFlag = "*"
		} else {
			continue
		}
		out += fmt.Sprintf("%s %3d %-40s%s\n", diffFlag, lineNumber+1, aLine, bLine)
	}

	return out
}
