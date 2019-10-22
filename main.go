package main

import (
	"bytes"
	"flag"
	"fmt"
	"go/format"
	"go/token"
	"io/ioutil"
	"os"
	"os/exec"
	"runtime"
	"strings"

	"github.com/Konstantin8105/f4go/fortran"
)

var (
	packageFlag string
	verboseFlag bool
)

func main() {
	pntPack := &packageFlag
	pntPack = flag.String("p", "main", "set the name of the generated package")
	_ = pntPack

	pntVerbose := &verboseFlag
	pntVerbose = flag.Bool("v", false, "verbose all stages of transpilation and all errors")
	_ = pntVerbose

	run()
}

func run() {
	// free 1 CPU for other computer stuff
	runtime.GOMAXPROCS(runtime.GOMAXPROCS(0) - 1)

	flag.Parse()

	if flag.NArg() < 1 {
		fmt.Fprintln(os.Stdout, "Please run: f4go -h")
		return
	}

	es := parseParallel(flag.Args(), packageFlag)
	if verboseFlag {
		for _, e := range es {
			fmt.Printf("%20s : %s\n", e.filename, e.err.Error())
		}
	}
}

type errorRow struct {
	err      error
	filename string
}

func (e errorRow) Error() string {
	return fmt.Sprintf("%s : %v", e.filename, e.err)
}

// parsing to Go code
func parse(filename, packageName, goFilename string) (errR []errorRow) {
	if packageName == "" {
		packageName = "main"
	}

	// read fortran source
	dat, err := ioutil.ReadFile(filename)
	if err != nil {
		return []errorRow{
			{
				err:      fmt.Errorf("Cannot fortran source: %v", err),
				filename: filename,
			},
		}
	}

	// remove some end line symbols
	dat = bytes.Replace(dat, []byte{'\r'}, []byte{}, -1)
	dat = bytes.Replace(dat, []byte{'\015'}, []byte{}, -1)

	// parse fortran to go/ast
	ast, errs := fortran.Parse(dat, packageName)
	if len(errs) > 0 {
		for _, er := range errs {
			errR = append(errR, errorRow{
				err:      fmt.Errorf("Parsing error : %v", er.Error()),
				filename: filename,
			})
		}
	}

	// convert ast tree to string
	var buf bytes.Buffer
	if err = format.Node(&buf, token.NewFileSet(), &ast); err != nil {
		return []errorRow{{err: fmt.Errorf("Error go/format : %v", err), filename: filename}}
	}

	// generate filename of result
	if goFilename == "" {
		index := strings.LastIndex(filename, ".")
		goFilename = filename + ".go"
		if index > 0 {
			goFilename = filename[:index] + ".go"
		}
	}

	// 	fmt.Println("\n\ngenerate : ", goFilename)

	// save go source
	if err = ioutil.WriteFile(goFilename, buf.Bytes(), 0644); err != nil {
		return []errorRow{{err: fmt.Errorf("Cannot write Go source: %v", err), filename: filename}}
	}

	// gofmt simplification
	_, _ = exec.Command("gofmt", "-s", "-w", goFilename).CombinedOutput()

	// goimports
	_, _ = exec.Command("goimport", "-w", goFilename).CombinedOutput()

	return
}

func parseParallel(filenames []string, packageName string) (ess []errorRow) {
	var (
		jobs    = make(chan string, len(filenames))
		results = make(chan []errorRow, len(filenames))
	)

	for w := 1; w <= 2*runtime.NumCPU(); w++ {
		go func(jobs <-chan string, results chan<- []errorRow) {
			for job := range jobs {
				results <- parse(job, packageName, "")
			}
		}(jobs, results)
	}

	for _, f := range filenames {
		jobs <- f
	}
	close(jobs)

	for range filenames {
		ess = append(ess, <-results...)
	}
	return
}
