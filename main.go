package main

import (
	"bytes"
	"flag"
	"fmt"
	"go/format"
	"go/token"
	"io/ioutil"
	"strings"

	"github.com/Konstantin8105/f4go/fortran"
)

func main() {
	packageFlag := flag.String(
		"p", "main", "set the name of the generated package")

	flag.Parse()

	if flag.NFlag() == 0 {
		flag.PrintDefaults()
	}

	fmt.Println("Input file:", *packageFlag, flag.Args())

	for _, inp := range flag.Args() {
		es := parse(inp, *packageFlag)
		for _, e := range es {
			fmt.Printf("%20s : %s\n", e.filename, e.err.Error())
		}
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
			errorRow{
				err:      fmt.Errorf("Cannot fortran source: %v", err),
				filename: filename,
			},
		}
	}

	// parse fortran to go/ast
	ast, errs := fortran.Parse(dat, packageName)
	if len(errs) > 0 {
		var e []errorRow
		for _, err := range errs {
			e = append(e, errorRow{
				err:      fmt.Errorf("Error: %v", err),
				filename: filename,
			})
		}
		return e
	}

	// convert ast tree to string
	var buf bytes.Buffer
	if err = format.Node(&buf, token.NewFileSet(), &ast); err != nil {
		return []errorRow{
			errorRow{
				err:      fmt.Errorf("Error go/format : %v", err),
				filename: filename,
			},
		}
	}

	// generate filename of result
	index := strings.LastIndex(filename, ".")
	goFilename := filename + ".go"
	if index > 0 {
		goFilename = filename[:index] + ".go"
	}

	// save go source
	if err = ioutil.WriteFile(goFilename, buf.Bytes(), 0644); err != nil {
		return []errorRow{
			errorRow{
				err:      fmt.Errorf("Cannot write Go source: %v", err),
				filename: filename,
			},
		}
	}

	return nil
}
