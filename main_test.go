// +build integration

package main

import (
	"bytes"
	"fmt"
	"go/format"
	"go/token"
	"io/ioutil"
	"os/exec"
	"strings"
	"testing"

	"github.com/Konstantin8105/f4go/fortran"
)

func TestIntegration(t *testing.T) {
	// gfortran ./testdata/main.f -o ./testdata/a.out
	out, err := exec.Command(
		"gfortran",
		"./testdata/main.f",
		"-o", "./testdata/a.out",
	).CombinedOutput()
	if err != nil {
		t.Fatalf("Cannot compile by gfortran: %v\n%s", err, out)
	}

	// ./testdata/a.out
	fortranOutput, err := exec.Command(
		"./testdata/a.out",
	).CombinedOutput()
	if err != nil {
		t.Fatalf("Cannot fortran executable file : %v\n%s", err, fortranOutput)
	}

	t.Logf("Fortran output:\n%s\n", fortranOutput)
	t.Logf("fortran source is ok")

	// parsing to Go code
	dat, err := ioutil.ReadFile("./testdata/main.f")
	if err != nil {
		t.Fatalf("Cannot fortran source: %v", err)
	}
	ast, errs := fortran.Parse(dat)
	if len(errs) > 0 {
		for _, err := range errs {
			t.Logf("Error: %v", err)
		}
		t.Fatal("Errors is more zero")
	}

	var buf bytes.Buffer
	if err = format.Node(&buf, token.NewFileSet(), &ast); err != nil {
		t.Fatal(err)
	}

	err = ioutil.WriteFile("./testdata/main.go", buf.Bytes(), 0644)
	if err != nil {
		t.Fatalf("Cannot write Go source: %v", err)
	}

	// run Go code
	goOutput, err := exec.Command(
		"go", "run", "./testdata/g.go",
	).CombinedOutput()
	if err != nil {
		t.Fatalf("Cannot go executable file : %v\n%s", err, goOutput)
	}

	if !bytes.Equal(fortranOutput, goOutput) {
		t.Errorf("Results is not same: `%v` != `%v`",
			string(fortranOutput),
			string(goOutput))
	}
}

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

func TestData(t *testing.T) {
	files, err := getFortranTestFiles("./testdata")
	if err != nil {
		t.Fatal(err)
	}

	for _, filename := range files {
		t.Run(fmt.Sprintf("%s", filename), func(t *testing.T) {
			errs := parse(filename, "")
			if len(errs) > 0 {
				for _, e := range errs {
					t.Error(e)
				}
				t.Fatal("Error is not empty")
			}
		})
	}
}

func BenchmarkCgemm(b *testing.B) {
	for i := 0; i < b.N; i++ {
		b.StopTimer()
		// read body of file
		d, err := ioutil.ReadFile("./testdata/blas/cgemm.f")
		if err != nil {
			b.Fatal(err)
		}

		b.StartTimer()
		_, _ = fortran.Parse(d)
	}
}
