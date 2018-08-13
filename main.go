package main

import (
	"bytes"
	"flag"
	"fmt"
	"go/format"
	"go/token"
	"io/ioutil"
	"runtime"
	"strings"

	"github.com/Konstantin8105/f4go/fortran"
)

var packageFlag *string

func main() {
	packageFlag = flag.String("p",
		"main", "set the name of the generated package")

	run()
}

func run() {
	flag.Parse()

	if flag.NFlag() == 0 {
		flag.PrintDefaults()
	}

	if packageFlag == nil {
		var s string
		packageFlag = &s
	}

	es := parseParallel(flag.Args(), *packageFlag)
	for _, e := range es {
		fmt.Printf("%20s : %s\n", e.filename, e.err.Error())
	}
}

type errorRow struct {
	err      error
	filename string
}

// parsing to Go code
func parse(filename, packageName string) []errorRow {

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

	// parse fortran to go/ast
	ast, errs := fortran.Parse(dat, packageName)
	if len(errs) > 0 {
		var e []errorRow
		for _, er := range errs {
			e = append(e, errorRow{
				err:      fmt.Errorf("Parsing error : %v", er.Error()),
				filename: filename,
			})
		}
		return e
	}

	// convert ast tree to string
	var buf bytes.Buffer
	if err = format.Node(&buf, token.NewFileSet(), &ast); err != nil {
		return []errorRow{{err: fmt.Errorf("Error go/format : %v", err), filename: filename}}
	}

	// generate filename of result
	index := strings.LastIndex(filename, ".")
	goFilename := filename + ".go"
	if index > 0 {
		goFilename = filename[:index] + ".go"
	}

	// save go source
	if err = ioutil.WriteFile(goFilename, buf.Bytes(), 0644); err != nil {
		return []errorRow{{err: fmt.Errorf("Cannot write Go source: %v", err), filename: filename}}
	}

	return nil
}

func parseParallel(filenames []string, packageName string) (ess []errorRow) {
	var (
		jobs    = make(chan string, len(filenames))
		results = make(chan []errorRow, len(filenames))
	)

	for w := 1; w <= 2*runtime.NumCPU(); w++ {
		go func(jobs <-chan string, results chan<- []errorRow) {
			for job := range jobs {
				results <- parse(job, packageName)
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
