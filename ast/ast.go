package ast

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func ParseAST(treeFile string) (nss [][]interface{}, err error) {
	file, err := os.Open(treeFile)
	if err != nil {
		return
	}
	defer file.Close()

	var block []string

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		if line[0] == ' ' {
			block[len(block)-1] += " " + strings.TrimSpace(line)
			continue
		}
		if line[0] == '@' {
			block = append(block, line)
			continue
		}
		var ns []interface{}
		ns, err = parseBlock(block)
		if err != nil {
			return
		}
		nss = append(nss, ns)
		block = []string{line}
	}

	if err := scanner.Err(); err != nil {
		return nil, err
	}

	return
}

func parseBlock(block []string) (ns []interface{}, err error) {
	if len(block) == 0 {
		return
	}
	fmt.Println("---------------------------------")

	for i, l := range block {

		if i == 0 {
			fmt.Println("NAME : ", l)
			continue
		}

		var n interface{}
		n, err = parse(l)
		if err != nil {
			return
		}
		ns = append(ns, n)
		// fmt.Printf("\n")
		// fmt.Println(l)
		// fmt.Println("n: ", n)

		/*
			rr := strings.Split(strings.TrimSpace(l), " ")
			for _, t := range rr {
				t := strings.TrimSpace(t)
				if t == "" {
					continue
				}
				fmt.Printf("|%s|\n", t)
			}
		*/

	}

	return
}

func parse(line string) (n interface{}, err error) {
	if line[0] != '@' {
		err = fmt.Errorf("Not @")
		return
	}
	var index int

	// go to space
	for {
		if line[index] == ' ' {
			break
		}
		index++
	}
	// n.link = line[:index]

	// go to not space
	for {
		if line[index] != ' ' {
			break
		}
		index++
	}
	begin := index
	// go to space
	for {
		if line[index] == ' ' {
			break
		}
		index++
	}
	// n.name = line[begin:index]
	//
	// ll := strings.Split(line[index+1:], ":")
	// for _, r := range ll {
	// 	rr := strings.Split(strings.TrimSpace(r), " ")
	// 	for _, t := range rr {
	// 		t := strings.TrimSpace(t)
	// 		if t == "" {
	// 			continue
	// 		}
	// 		fmt.Println(t)
	// 	}
	// }

	p := map[string]func(string) interface{}{
		"identifier_node":       parse_identifier_node,
		"integer_cst":           parse_integer_cst,
		"type_decl":             parse_type_decl,
		"function_decl":         parse_function_decl,
		"integer_type":          parse_integer_type,
		"function_type":         parse_function_type,
		"tree_list":             parse_tree_list,
		"void_type":             parse_void_type,
		"var_decl":              parse_var_decl,
		"modify_expr":           parse_modify_expr,
		"bind_expr":             parse_bind_expr,
		"translation_unit_decl": parse_translation_unit_decl,
	}

	if f, ok := p[line[begin:index]]; ok {
		n = f(line[index:])
	} else {
		fmt.Println("Undefined:",
			line[begin:index],
			"\t",
			line[index:])
	}

	// for {
	// 	begin = index
	// 	// go to :
	// 	for {
	// 		if index == len(line) {
	// 			return
	// 		}
	// 		if line[index] == ':' {
	// 			break
	// 		}
	// 		index++
	// 	}
	// 	if index-begin == 0 {
	// 		index++
	// 		continue
	// 	}
	// 	var p property
	// 	p.key = line[begin:index]
	// 	p.value = line[index:]
	// 	fmt.Println("---- ", p.key, " ----- ", p.value)
	//
	// 	n.properties = append(n.properties, p)
	// }

	return
}
