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

	fortran.Debug = testing.Verbose()

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

	if err := ioutil.WriteFile("./testdata/result", fortranOutput, 0644); err != nil {
		t.Error("Cannot store result")
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
		out += fmt.Sprintf("%s %3d %2d/%2d %-60s%s\n", diffFlag, lineNumber+1,
			len(aLine), len(bLine),
			aLine, bLine)
	}
	return out
}

func TestFail(t *testing.T) {
	t.Skip()

	fortran.Debug = testing.Verbose()

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
			!strings.HasSuffix(ent.Name(), ".src") &&
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

func parsingBlas(filename string) error {
	filename = "./" + filename

	// Go name
	goname := filename
	if index := strings.LastIndex(goname, "."); index > 0 {
		goname = goname[:index] + ".go"
	}
	goname = strings.Replace(goname, "SRC", "TESTING", 1)

	// parse
	errs := parse(filename, "main", goname)
	if len(errs) == 0 {
		return nil
	}

	var s string
	for i := range errs {
		s += errs[i].Error() + "\n"
	}
	return fmt.Errorf("%s", s)
}

func TestFeappv(t *testing.T) {
	fortran.Debug = testing.Verbose()

	ss, err := filepath.Glob(fmt.Sprintf("./testdata/feappv-master/plot/%s", "*.f"))
	if err != nil || len(ss) == 0 {
		t.Fatal(err)
	}
	ss = []string{"./testdata/feappv-master/plot/fpplcl.f"} // TODO remove

	var amount int

	for i := range ss {
		err = parsingBlas(ss[i])
		if err != nil {
			t.Logf("Error is not empty in file: %s. %v", ss[i], err)
			amount++
		}
	}

	if float64(amount) > 0.25*float64(len(ss)) {
		t.Errorf("too mush errors")
	}
}

func TestLapack(t *testing.T) {

	fortran.Debug = testing.Verbose()

	ss, err := filepath.Glob(fmt.Sprintf("./testdata/lapack/SRC/%s", "*.f"))
	if err != nil || len(ss) == 0 {
		t.Fatal(err)
	}

	var amount int

	for i := range ss {
		err = parsingBlas(ss[i])
		if err != nil {
			t.Logf("Error is not empty in file: %s. %v", ss[i], err)
			amount++
		}
	}

	if float64(amount) > 0.25*float64(len(ss)) {
		t.Errorf("too mush errors")
	}
}

func TestBlas(t *testing.T) {
	t.Skip()

	fortran.Debug = testing.Verbose()

	ss, err := filepath.Glob(fmt.Sprintf("./testdata/lapack/BLAS/SRC/%s", "*.f"))
	if err != nil || len(ss) == 0 {
		t.Fatal(err)
	}
	{
		// add TESTING
		ss2, err := filepath.Glob(fmt.Sprintf("./testdata/lapack/BLAS/TESTING/%s", "*.f"))
		if err != nil || len(ss) == 0 {
			t.Fatal(err)
		}
		ss = append(ss, ss2...)
	}

	var amount int

	for i := range ss {
		err := parsingBlas(ss[i])
		if err != nil {
			t.Logf("Error is not empty in file: %s. %v", ss[i], err)
			amount++
		}
	}

	if float64(amount) > 0.25*float64(len(ss)) {
		t.Errorf("too mush errors")
	}

	// check README
	lines := func(file string) []string {
		d, err := ioutil.ReadFile(file)
		if err != nil {
			t.Fatal(err)
		}
		return strings.Split(string(d), "\n")
	}

	// get lines of README.md
	readme := lines("./README.md")

	// get lines of source fortran file
	fortran := lines("./testdata/lapack/BLAS/SRC/caxpy.f")
	for i := range fortran {
		found := false
		for j := range readme {
			if fortran[i] == readme[j] {
				found = true
				break
			}
		}
		if !found {
			t.Errorf("Cannot find fortran line %d: %s", i, fortran[i])
		}
	}

	// get lines of Go file
	gof := lines("./testdata/lapack/BLAS/TESTING/caxpy.go")
	for i := range gof {
		found := false
		for j := range readme {
			if gof[i] == readme[j] {
				found = true
				break
			}
		}
		if !found {
			t.Errorf("Cannot find go line %d: %s", i, gof[i])
		}
	}
}

func TestBlasTesting(t *testing.T) {
	isFull := os.Getenv("FULL") != ""

	if !isFull {
		return
	}

	tcs := []struct {
		fortranFiles []string
		goFiles      []string
	}{
		{
			fortranFiles: []string{
				"./testdata/lapack/BLAS/TESTING/cblat1.f", // testing file
				"./testdata/lapack/BLAS/SRC/csscal.f",
				"./testdata/lapack/BLAS/SRC/cdotc.f",
				"./testdata/lapack/BLAS/SRC/cdotu.f",
				"./testdata/lapack/BLAS/SRC/cscal.f",
				"./testdata/lapack/BLAS/SRC/scnrm2.f",
				"./testdata/lapack/BLAS/SRC/icamax.f",
				"./testdata/lapack/BLAS/SRC/caxpy.f",
				"./testdata/lapack/BLAS/SRC/scasum.f",
				"./testdata/lapack/BLAS/SRC/ccopy.f",
				"./testdata/lapack/BLAS/SRC/cswap.f",
				"./testdata/lapack/BLAS/SRC/scabs1.f",
			},
		},
	}

	// generate go names
	for i := range tcs {
		for j := range tcs[i].fortranFiles {
			goname := tcs[i].fortranFiles[j]
			index := strings.LastIndex(goname, ".")
			goname = goname[:index] + ".go"
			goname = strings.Replace(goname, "SRC", "TESTING", -1)
			tcs[i].goFiles = append(tcs[i].goFiles, goname)
		}
	}

	for i := range tcs {
		t.Run(fmt.Sprintf("TESTING%3d", i), func(t *testing.T) {
			for j := range tcs[i].fortranFiles {
				err := parsingBlas(tcs[i].fortranFiles[j])
				if err != nil {
					t.Logf("failed file: %s. %v", tcs[i].fortranFiles[j], err)
				}
			}

			args := []string{"build", "-gcflags", "-e"}
			//		run Go test
			cmd := exec.Command("go", append(args, tcs[i].goFiles...)...)
			goOutput, err := cmd.CombinedOutput()
			if err != nil {
				t.Errorf("Cannot go executable file : %v\n%s", err, goOutput)
			}
		})
	}

}

func TestData(t *testing.T) {
	files, err := getFortranTestFiles("./testdata/other")
	fmt.Println(files, err)
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
		d, err := ioutil.ReadFile("./testdata/lapack/BLAS/cgemm.f")
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
