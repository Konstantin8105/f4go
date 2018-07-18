package main_test

import (
	"bytes"
	"go/format"
	"go/token"
	"path/filepath"
	"testing"

	"github.com/Konstantin8105/f4go/fortran"
	"github.com/Konstantin8105/f4go/util"
)

func TestOk(t *testing.T) {
	files, err := filepath.Glob("testdata/*.f")
	if err != nil {
		t.Fatalf("Cannot found files : %v", err)
	}
	for _, filename := range files {
		t.Run(filename, func(t *testing.T) {
			ast, err := fortran.Parse(filename)

			var buf bytes.Buffer
			if err = format.Node(&buf, token.NewFileSet(), &ast); err != nil {
				return
			}

			filename += ".go"
			var out string
			out, err = util.IsDiff(filename, buf.String())
			if err != nil {
				t.Fatal(err)
			}
			if out != "" {
				t.Fatal(out)
			}
		})
	}
}
