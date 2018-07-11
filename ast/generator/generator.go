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
	arg := strings.TrimSpace(os.Args[1])
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

	var output *os.File
	if _, err := os.Stat("ast/" + filename); err == nil {
		output = os.Stdout
	} else {
		output, err = os.Create("ast/" + filename)
		if err != nil {
			fmt.Println("err = ", err)
			return
		}
	}

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
	fmt.Fprintln(output, `
package ast

import "strings"
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
`, structName, structName)

	// create function
	fmt.Fprintf(output, "func parse_%s(line string) (n Node) {\n", identificator)

	// create a group
	fmt.Fprintf(output, "\tgroups := groupsFromRegex(\n\t`\n")
	for _, t := range types {
		fmt.Fprintf(output, "\t%v:(?P<%v>.*) +\n", t, t)
	}
	fmt.Fprintf(output, "\t`,\n\tline,\n\t)\n")

	// create return struct
	fmt.Fprintf(output, "\treturn %v{\n", structName)
	for _, t := range types {
		fmt.Fprintf(output, "\t\t%v: strings.TrimSpace(groups[\"%v\"]),\n",
			firstUpper(t), t)
	}
	fmt.Fprintf(output, "\t}\n")

	// end of function
	fmt.Fprintln(output, "}")

	//
	fmt.Println("=====================")
	fmt.Printf("\"%v\":parse_%v,\n", identificator, identificator)
}

func firstUpper(s string) string {
	return strings.ToUpper(string(s[0])) + s[1:]
}

func check(v string) string {
	return strings.TrimSpace(v)
}
