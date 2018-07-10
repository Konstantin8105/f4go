package ast

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func ParseAST(treeFile string) (nss [][]Node, err error) {
	file, err := os.Open(treeFile)
	if err != nil {
		return
	}
	defer file.Close()

	var block []string

	scanner := bufio.NewScanner(file)

	var ns []Node
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
		// if len(block) == 0 {
		// 	block = []string{line}
		// 	continue
		// }
		ns, err = parseBlock(block)
		fmt.Println("err = ", err)
		if err != nil {
			return
		}
		nss = append(nss, ns)
		block = []string{line}
	}
	ns, err = parseBlock(block)
	fmt.Println("err = ", err)
	if err != nil {
		return
	}
	nss = append(nss, ns)

	if err := scanner.Err(); err != nil {
		return nil, err
	}

	return
}

func parseBlock(block []string) (ns []Node, err error) {
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

	p := map[string]func(string) Node{
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
		"pointer_type":          parse_pointer_type,

		"result_decl":       parse_result_decl,
		"parm_decl":         parse_parm_decl,
		"addr_expr":         parse_addr_expr,
		"return_expr":       parse_return_expr,
		"call_expr":         parse_call_expr,
		"array_ref":         parse_array_ref,
		"array_type":        parse_array_type,
		"constructor":       parse_constructor,
		"reference_type":    parse_reference_type,
		"record_type":       parse_record_type,
		"real_type":         parse_real_type,
		"field_decl":        parse_field_decl,
		"mult_expr":         parse_mult_expr,
		"plus_expr":         parse_plus_expr,
		"boolean_type":      parse_boolean_type,
		"loop_expr":         parse_loop_expr,
		"label_decl":        parse_label_decl,
		"nop_expr":          parse_nop_expr,
		"cond_expr":         parse_cond_expr,
		"le_expr":           parse_le_expr,
		"float_expr":        parse_float_expr,
		"goto_expr":         parse_goto_expr,
		"trunc_div_expr":    parse_trunc_div_expr,
		"eq_expr":           parse_eq_expr,
		"label_expr":        parse_label_expr,
		"statement_list":    parse_statement_list,
		"component_ref":     parse_component_ref,
		"real_cst":          parse_real_cst,
		"pointer_plus_expr": parse_pointer_plus_expr,
		"indirect_ref":      parse_indirect_ref,
		"fix_trunc_expr":    parse_fix_trunc_expr,
		"const_decl":        parse_const_decl,
		"string_cst":        parse_string_cst,
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
