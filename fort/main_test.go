package fort

import (
	"fmt"
	"io/ioutil"
	"testing"
)

func TestScanner(t *testing.T) {
	dat, err := ioutil.ReadFile("position.go")
	if err != nil {
		t.Fatal(err)
	}

	// Initialize the scanner.
	var s Scanner
	fset := NewFileSet()                                       // positions are relative to fset
	file := fset.AddFile("position.go", fset.Base(), len(dat)) // register input "file"
	s.Init(file, dat, nil /* no error handler */, ScanComments)

	// Repeated calls to Scan yield the token sequence found in the input.
	for {
		pos, tok, lit := s.Scan()
		if tok == EOF {
			break
		}
		fmt.Printf("%s\t%s\t%q\n", fset.Position(pos), tok, lit)
	}
}
