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

	"github.com/bradleyjkemp/cupaloy"
)

func TestScanner(t *testing.T) {

	files, err := filepath.Glob("../testdata/*.f")
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

			// Update tests
			// UPDATE_SNAPSHOTS=true go test ./fortran/...
			err = cupaloy.SnapshotMulti(testName, buf.String())
			if err != nil {
				t.Fatalf("error: %s", err)
			}
		})
	}

}
