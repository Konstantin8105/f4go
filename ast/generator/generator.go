package main

import (
	"fmt"
	"os"
	"strings"
)

func main() {
	if len(os.Args) < 1 {
		fmt.Println("Arguments not found")
		return
	}
	arg := os.Args[1]
	fmt.Printf("Take argument: `%v`\n", arg)

	// find filename
	index := strings.Index(arg, " ")
	if index < 1 {
		fmt.Println("Filename is not found")
		return
	}
	identificator := arg[:index]
	structName := firstUpper(arg[:index])
	filename := arg[:index] + ".go"
	fmt.Printf("Filename: `%v`\n", filename)

	var types []string
	// find all types
	for {
		indexNew := strings.Index(arg[index:], ":")
		if indexNew < 0 {
			fmt.Println("End of type searching")
			break
		}
		index += indexNew + 1
		begin := index - 5
		fmt.Println("Type : ", arg[begin:index-1])
		types = append(types, arg[begin:index-1])
	}

	// check all types
	for i := range types {
		types[i] = check(types[i])
	}

	fmt.Println("=====================")

	// create header
	fmt.Println(`
package ast

import "strings"
`)

	// create struct
	fmt.Printf("type %s struct {\n", structName)
	for _, t := range types {
		fmt.Printf("\t%v string\n", firstUpper(t))
	}
	fmt.Println("}\n")

	// create function
	fmt.Printf("func parse_%s(line string) (n interface{}) {\n", identificator)

	// create a group
	fmt.Printf("\tgroups := groupsFromRegex(\n\t`\n")
	for _, t := range types {
		fmt.Printf("\t%v:(?P<%v>.*) +\n", t, t)
	}
	fmt.Printf("\t`,\n\tline,\n\t)\n")

	// create return struct
	fmt.Printf("\treturn %v{\n", structName)
	for _, t := range types {
		fmt.Printf("\t\t%v: strings.TrimSpace(groups[\"%v\"]),\n",
			firstUpper(t), t)
	}
	fmt.Printf("\t}\n")

	// end of function
	fmt.Println("}")

	//
	fmt.Println("==================")
	fmt.Printf("\"%v\":parse_%v,\n", identificator, identificator)
}

func firstUpper(s string) string {
	return strings.ToUpper(string(s[0])) + s[1:]
}

func check(v string) string {
	return strings.TrimSpace(v)
}
