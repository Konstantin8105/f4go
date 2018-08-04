package main

import (
	"flag"
	"fmt"
)

func main() {
	inputFilename := flag.String("i", "", "input fortran source")

	flag.Parse()

	if *inputFilename == "" {
		flag.PrintDefaults()
		return
	}

	fmt.Println("Input file:", *inputFilename)
}
