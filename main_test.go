package main_test

import (
	"bytes"
	"fmt"
	"go/format"
	"go/token"
	"io/ioutil"
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
			if err != nil {
				t.Fatal(err)
			}

			var buf bytes.Buffer
			if err = format.Node(&buf, token.NewFileSet(), &ast); err != nil {
				t.Fatal(err)
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

func getFortranTestFiles() (files []string, err error) {
	locations := []string{
		"testdata/other/*.f",
		"testdata/other/*.f90",
		"testdata/ftnchek/*.f",
		"testdata/ftnchek/*.f90",
		"testdata/blas/*.f",
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

func TestFail(t *testing.T) {
	files, err := getFortranTestFiles()
	if err != nil {
		t.Fatalf("Cannot found files : %v", err)
	}
	for _, filename := range files {
		t.Run(filename, func(t *testing.T) {
			defer func() {
				if r := recover(); r != nil {
					fmt.Println(r)
				}
			}()
			ast, err := fortran.Parse(filename)
			if err != nil {
				return
			}

			var buf bytes.Buffer
			if err = format.Node(&buf, token.NewFileSet(), &ast); err != nil {
				return
			}

			filename += ".go"
			err = ioutil.WriteFile(filename, buf.Bytes(), 0644)
			if err != nil {
				return
			}
		})
	}
}
