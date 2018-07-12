package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"sort"
	"strings"

	"github.com/Konstantin8105/f4go/ast"
)

func main() {
	// get all blocks
	files, err := filepath.Glob("testdata/*.out")
	if err != nil {
		fmt.Println("err = ", err)
		return
	}

	var bigBlock []string
	for _, file := range files {
		blocks, err := ast.GetBlocks(file)
		if err != nil {
			fmt.Println("err = ", err)
			return
		}
		for i := range blocks {
			if len(blocks[i]) < 1 {
				continue
			}
			for j := range blocks[i] {
				s := strings.TrimSpace(blocks[i][j])
				if len(s) < 2 {
					continue
				}
				if s[0] != '@' {
					continue
				}
				index := strings.Index(s, " ")
				s = strings.TrimSpace(s[index:])
				bigBlock = append(bigBlock, s)
			}
		}
	}

	sort.Strings(bigBlock)

	m := map[string]map[string]bool{}

	for i := range bigBlock {
		ts := getType(bigBlock[i])
		if i == 0 {
			continue
		}
		index := strings.Index(bigBlock[i], " ")
		if index < 0 {
			// no types
			checkStruct(bigBlock[i], []string{})
			continue
		}
		name := bigBlock[i][:index]
		filename := "ast/" + name + ".go"
		if _, err := os.Stat(filename); err != nil {
			if m[name] == nil {
				m[name] = map[string]bool{}
			}
			for _, t := range ts {
				m[name][t] = true
			}
			continue
		}
		checkStruct(name, ts)
	}

	// create structs
	for k := range m {
		name := k
		var types []string
		for t := range m[k] {
			types = append(types, t)
		}
		sort.Strings(types)
		createStruct(name, types)
	}
}

func checkStruct(name string, types []string) {
	filename := "ast/" + name + ".go"

	dat, err := ioutil.ReadFile(filename)
	if err != nil {
		fmt.Println("err = ", err)
		return
	}

	pattern := "findVal(\""

	bs := bytes.Split(dat, []byte(pattern))

	var actualTypes []string
	for i := 1; i < len(bs); i++ {
		actualTypes = append(actualTypes, string(bs[i][:4]))
	}

	for _, t := range types {
		var found bool
		for i := range actualTypes {
			if t == actualTypes[i] {
				found = true
			}
		}
		if !found {
			fmt.Println(">>> ", name, ": Not found :", t)
		}
	}

}

// find all types
func getType(arg string) (types []string) {
	index := strings.Index(arg, " ")
	for index > 0 {
		indexNew := strings.Index(arg[index:], ":")
		if indexNew < 0 {
			// fmt.Println("End of type searching")
			break
		}
		index += indexNew + 1
		begin := index - 5
		name := arg[begin : index-1]
		if strings.ContainsAny(name, ".:<>") {
			continue
		}
		// fmt.Printf("Type : `%s`\n", name)
		types = append(types, name)
	}
	return
}

func createStruct(name string, types []string) {
	filename := name + ".go"
	output, err := os.Create("ast/" + filename)
	if err != nil {
		fmt.Println("err = ", err)
		return
	}

	identificator := name
	structName := firstUpper(identificator)

	// create header
	fmt.Fprintln(output, `
		package ast

		`)

	// create struct
	fmt.Fprintf(output, "type %s struct {\n", structName)
	for _, t := range types {
		fmt.Fprintf(output, "\t%v string\n", firstUpper(t))
	}
	fmt.Fprintln(output, "}\n")

	// create method
	fmt.Fprintf(output,
		`
		func (a %v) GenNodeName() string {
			return "%v"
		}
		`, structName, identificator)

	// create function
	fmt.Fprintf(output, "func parse_%s(line string) (n Node) {\n", identificator)

	// create return struct
	fmt.Fprintf(output, "\treturn %v{\n", structName)
	for i, t := range types {
		fmt.Fprintf(output, "\t\t%v: findVal(\"%v:\", &line),\n",
			firstUpper(t), types[i])
	}
	fmt.Fprintf(output, "\t}\n")

	// end of function
	fmt.Fprintln(output, "}")

	//
	fmt.Println("=====================")
	fmt.Printf("\"%v\":parse_%v,\n", identificator, identificator)
}

func firstUpper(s string) (out string) {
	out = s
	out = strings.Replace(out, " ", "", -1)
	return strings.ToUpper(string(out[0])) + out[1:]
}
