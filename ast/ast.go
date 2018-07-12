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

		ns, err = parseBlock(block)
		if err != nil {
			return
		}
		nss = append(nss, ns)
		block = []string{line}
	}
	ns, err = parseBlock(block)
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

	for i, l := range block {
		if i == 0 {
			continue
		}

		var n Node
		n, err = parse(l)
		if err != nil {
			return
		}
		ns = append(ns, n)
	}

	return
}

func parse(line string) (n Node, err error) {
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

	p := map[string]func(string) Node{
		"addr_expr":         parse_addr_expr,
		"bind_expr":         parse_bind_expr,
		"call_expr":         parse_call_expr,
		"cond_expr":         parse_cond_expr,
		"eq_expr":           parse_eq_expr,
		"fix_trunc_expr":    parse_fix_trunc_expr,
		"float_expr":        parse_float_expr,
		"goto_expr":         parse_goto_expr,
		"label_expr":        parse_label_expr,
		"le_expr":           parse_le_expr,
		"loop_expr":         parse_loop_expr,
		"minus_expr":        parse_minus_expr,
		"modify_expr":       parse_modify_expr,
		"mult_expr":         parse_mult_expr,
		"nop_expr":          parse_nop_expr,
		"plus_expr":         parse_plus_expr,
		"pointer_plus_expr": parse_pointer_plus_expr,
		"return_expr":       parse_return_expr,
		"trunc_div_expr":    parse_trunc_div_expr,

		"array_type":     parse_array_type,
		"boolean_type":   parse_boolean_type,
		"function_type":  parse_function_type,
		"integer_type":   parse_integer_type,
		"pointer_type":   parse_pointer_type,
		"real_type":      parse_real_type,
		"record_type":    parse_record_type,
		"reference_type": parse_reference_type,
		"void_type":      parse_void_type,

		"const_decl":            parse_const_decl,
		"field_decl":            parse_field_decl,
		"function_decl":         parse_function_decl,
		"label_decl":            parse_label_decl,
		"parm_decl":             parse_parm_decl,
		"result_decl":           parse_result_decl,
		"translation_unit_decl": parse_translation_unit_decl,
		"type_decl":             parse_type_decl,
		"var_decl":              parse_var_decl,

		"array_ref":     parse_array_ref,
		"component_ref": parse_component_ref,
		"indirect_ref":  parse_indirect_ref,

		"constructor": parse_constructor,

		"identifier_node": parse_identifier_node,

		"integer_cst": parse_integer_cst,
		"real_cst":    parse_real_cst,
		"string_cst":  parse_string_cst,

		"statement_list": parse_statement_list,
		"tree_list":      parse_tree_list,
	}

	if f, ok := p[line[begin:index]]; ok {
		n = f(line[index:])
	} else {
		fmt.Println("Undefined:",
			line[begin:index],
			"\t",
			line[index:])
	}

	return
}
