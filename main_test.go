package main

import (
	"bufio"
	"bytes"
	"fmt"
	"go/parser"
	"go/scanner"
	"go/token"
	"io/ioutil"
	"log"
	"math"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
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

	// t.Logf("Fortran output:\n%s\n", fortranOutput)
	// t.Logf("fortran source is ok")

	// parsing to Go code
	errs := parse("./testdata/main.f", "", "")
	if len(errs) > 0 {
		for _, er := range errs {
			t.Logf("Error: %20s %v", er.filename, er.err.Error())
		}
		t.Errorf("Errors in parsing Go code is more zero")
	}

	// run Go code
	goOutput, err := exec.Command(
		"go", "run", "./testdata/main.go",
	).CombinedOutput()
	if err != nil {
		t.Fatalf("Cannot go executable file : %v\n%s", err, goOutput)
	}

	if !bytes.Equal(fortranOutput, goOutput) {
		t.Errorf(ShowDiff(string(fortranOutput), string(goOutput)))
		fortLines := bytes.Split(fortranOutput, []byte("\n"))
		goLines := bytes.Split(goOutput, []byte("\n"))
		if len(fortLines) != len(goLines) {
			t.Error("Amount lines is not same")
		}
		length := len(fortLines)
		if length > len(goLines) {
			length = len(goLines)
		}
		for i := 0; i < length; i++ {
			if !bytes.Equal(fortLines[i], goLines[i]) {
				t.Errorf("Results is not same in line %d: `%v`\n`%v`",
					i,
					string(fortLines[i]),
					string(goLines[i]))
				break
			}
		}
	}
}

// ShowDiff will print two strings vertically next to each other so that line
// differences are easier to read.
func ShowDiff(a, b string) string {
	aLines := strings.Split(a, "\n")
	bLines := strings.Split(b, "\n")
	maxLines := int(math.Max(float64(len(aLines)), float64(len(bLines))))
	out := "\n"

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
		}
		out += fmt.Sprintf("%s %3d %-60s%s\n", diffFlag, lineNumber+1, aLine, bLine)
	}
	return out
}

func TestFail(t *testing.T) {
	// wrong source
	errs := parse("./testdata/fortran_fail.f", "", "")
	if len(errs) == 0 {
		t.Error("Error is empty")
	}
	// run fail
	os.Args = []string{"", "./testdata/fortran_fail.f"}
	run()
	// wrong input data
	errs = parse("./testdata/sdfelmsdsdfsdfsdf.f", "", "")
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
	isFull := os.Getenv("FULL") != ""

	if !isFull {
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

func TestBlas(t *testing.T) {
	ss, err := filepath.Glob(fmt.Sprintf("./testdata/blas/%s", "*.f"))
	if err != nil {
		t.Fatal(err)
	}

	for i := range ss {
		t.Run(ss[i], func(t *testing.T) {
			ss[i] = "./" + ss[i]
			// generate filename of result
			index := strings.LastIndex(ss[i], "/")
			indexPoint := strings.LastIndex(ss[i][index:], ".")
			goFilename := ss[i][:index+1] +
				"blas/" +
				ss[i][index+1:index+indexPoint] +
				".go"
			// parse
			es := parse(ss[i], "main", goFilename)
			for _, e := range es {
				fmt.Printf("%20s : %s\n", e.filename, e.err.Error())
			}
			if len(es) > 0 {
				t.Fatal("Error is not empty")
			}
		})
	}
	// run Go test
	cmd := exec.Command(
		"go", "test", "-v", "-gcflags=-e", "-run=main_test.go",
	)
	cmd.Dir = "./testdata/blas/blas/"
	goOutput, err := cmd.CombinedOutput()
	if err != nil {
		t.Logf("Cannot go executable file : %v\n%s", err, goOutput)
	}
}

func TestData(t *testing.T) {
	files, err := getFortranTestFiles("./testdata/other")
	if err != nil {
		t.Fatal(err)
	}
	for i := range files {
		t.Run(files[i], func(t *testing.T) {
			es := parse(files[i], "", "")
			for _, e := range es {
				fmt.Printf("%20s : %s\n", e.filename, e.err.Error())
			}
			if len(es) > 0 {
				t.Fatal("Error is not empty")
			}
			// generate filename of result
			index := strings.LastIndex(files[i], ".")
			goFilename := files[i] + ".go"
			if index > 0 {
				goFilename = files[i][:index] + ".go"
			}
			// parse for errors
			_, err := parser.ParseFile(token.NewFileSet(), goFilename, nil, parser.AllErrors)
			if err != nil {
				if e, ok := err.(scanner.ErrorList); ok {
					for i := range e {
						if i > 20 {
							continue
						}
						t.Log(e[i])
					}
				}
				t.Fatal(fmt.Errorf("%v", err))
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
	os.Args = append([]string{}, "./testdata/main.f")
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

func TestTodo(t *testing.T) {
	// Show all todos in code
	s1, err := filepath.Glob(fmt.Sprintf("./%s", "*.go"))
	if err != nil {
		t.Fatal(err)
	}
	s2, err := filepath.Glob(fmt.Sprintf("./fortran/%s", "*.go"))
	if err != nil {
		t.Fatal(err)
	}
	source := []string{}
	source = append(source, s1...)
	source = append(source, s2...)

	var amount int

	for i := range source {
		t.Run(source[i], func(t *testing.T) {
			file, err := os.Open(source[i])
			if err != nil {
				t.Fatal(err)
			}
			defer file.Close()

			pos := 1
			scanner := bufio.NewScanner(file)
			for scanner.Scan() {
				line := scanner.Text()
				pos++
				if !strings.Contains(line, "//") {
					continue
				}
				if !strings.Contains(line, "TODO") {
					continue
				}
				index := strings.Index(line, "//")
				t.Logf("%d %s", pos, line[index:])
				amount++
			}

			if err := scanner.Err(); err != nil {
				log.Fatal(err)
			}
		})
	}
	t.Logf("Amount TODO: %d", amount)
}

func TestComments(t *testing.T) {
	// Show all todos in code
	s1, err := filepath.Glob(fmt.Sprintf("./%s", "*.go"))
	if err != nil {
		t.Fatal(err)
	}
	s2, err := filepath.Glob(fmt.Sprintf("./fortran/%s", "*.go"))
	if err != nil {
		t.Fatal(err)
	}
	source := []string{}
	source = append(source, s1...)
	source = append(source, s2...)

	var amount int

	for i := range source {
		t.Run(source[i], func(t *testing.T) {
			file, err := os.Open(source[i])
			if err != nil {
				t.Fatal(err)
			}
			defer file.Close()

			pos := 1
			scanner := bufio.NewScanner(file)
			for scanner.Scan() {
				line := scanner.Text()
				pos++
				if !strings.Contains(line, "/"+"*") {
					continue
				}
				index := strings.Index(line, "/"+"*")
				t.Logf("%d %s", pos, line[index:])
				amount++
			}

			if err := scanner.Err(); err != nil {
				log.Fatal(err)
			}
		})
	}
	t.Logf("Amount comments: %d", amount)
}
