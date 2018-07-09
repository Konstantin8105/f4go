package ast

import "strings"

type integer_cst struct {
	varType string
	varInt  string
}

func parse_integer_cst(line string) (n interface{}) {
	groups := groupsFromRegex(
		"type:(?P<type>.*)int:(?P<int>.*)",
		line,
	)

	return integer_cst{
		varType: strings.TrimSpace(groups["type"]),
		varInt:  strings.TrimSpace(groups["int"]),
	}
}
