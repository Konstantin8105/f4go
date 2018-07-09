package ast

import "strings"

type Integer_cst struct {
	VarType string
	VarInt  string
}

func parse_integer_cst(line string) (n interface{}) {
	groups := groupsFromRegex(
		"type:(?P<type>.*)int:(?P<int>.*)",
		line,
	)

	return Integer_cst{
		VarType: strings.TrimSpace(groups["type"]),
		VarInt:  strings.TrimSpace(groups["int"]),
	}
}
