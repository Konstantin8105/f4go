package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"os"
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
	errs := parse("./testdata/main.f", "")
	if len(errs) > 0 {
		for _, er := range errs {
			t.Logf("Error: %20s %v", er.filename, er.err.Error())
		}
		t.Fatal("Errors inparsing Go code is more zero")
	}

	// run Go code
	goOutput, err := exec.Command(
		"go", "run", "./testdata/main.go",
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

func TestFail(t *testing.T) {
	// wrong source
	errs := parse("./testdata/fortran_fail.f", "")
	if len(errs) == 0 {
		t.Error("Error is empty")
	}
	// run fail
	os.Args = []string{"", "./testdata/fortran_fail.f"}
	run()
	// wrong input data
	errs = parse("./testdata/sdfelmsdsdfsdfsdf.f", "")
	if len(errs) == 0 {
		t.Error("Error is empty")
	}
	// run fail
	os.Args = []string{"", "./testdata/sdfelmsdsdfsdfsdf.f"}
	run()
	os.Args = []string{""}
	run()
}

func getFortranTestFiles(dir string) (files []string, err error) {
	isFull := os.Getenv("FULL") == ""

	if isFull && strings.Contains(dir, "other") {
		return
	}

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
		// ignore fail files
		if strings.Contains(ent.Name(), "_fail") {
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
	for i := range files {
		t.Run(files[i], func(t *testing.T) {
			es := parse(files[i], "")
			for _, e := range es {
				fmt.Printf("%20s : %s\n", e.filename, e.err.Error())
			}
			if len(es) > 0 {
				t.Fatal("Error is not empty")
			}
		})
	}
}

func TestParallel(t *testing.T) {
	files, err := getFortranTestFiles("./testdata")
	if err != nil {
		t.Fatal(err)
	}

	es := parseParallel(files[:len(files)/2], "")
	if len(es) > 0 {
		t.Errorf("Error is not empty: %v", es)
	}
}

func TestRun(t *testing.T) {
	files, err := getFortranTestFiles("./testdata")
	if err != nil {
		t.Fatal(err)
	}

	os.Args = append([]string{}, files[:len(files)/3]...)
	run()
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
		_, _ = fortran.Parse(d, "")
	}
}
