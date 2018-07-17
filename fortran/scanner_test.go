package fortran

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"testing"
)

func TestScanner(t *testing.T) {

	files, err := filepath.Glob("../testdata/*.f")
	if err != nil {
		fmt.Println("err = ", err)
		return
	}

	for _, filename := range files {
		t.Run(filename, func(t *testing.T) {
			file, err := os.Open(filename)
			if err != nil {
				t.Fatal(err)
				return
			}
			defer file.Close()

			s := NewScanner(bufio.NewReader(file))
			for {
				tok, lit := s.Scan()
				if tok == ILLEGAL || tok == EOF {
					fmt.Println("->", lit)
					break
				}
				fmt.Printf("%v\t%v\n", tok, lit)
			}

		})
	}

}
