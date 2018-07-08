package main

import (
	"fmt"
	"os/exec"
	"path/filepath"
	"testing"
)

// Example : go test -v -run=TestParse/testdata/hw2.f

func TestParse(t *testing.T) {

	files, err := filepath.Glob("testdata/*.f")
	if err != nil {
		fmt.Println("err = ", err)
		return
	}

	for _, file := range files {
		t.Run(file, func(t *testing.T) {
			err = transpile(file)
			if err != nil {
				t.Fatal(err)
			}

			err = exec.Command("gfortran",
				file).Run()
			if err != nil {
				t.Fatal(err)
			}

			var out []byte
			out, err = exec.Command("./a.out").CombinedOutput()
			if err != nil {
				t.Fatal(err)
			}

			fmt.Println(string(out))
		})
	}
}
