package ast

import "strings"

type Integer_cst struct {
	VarType string
	VarInt  string
}

func (a Integer_cst) GenNodeName() string {
	return "Integer_cst "
}

func parse_integer_cst(line string) (n Node) {
	groups := groupsFromRegex(
		"type:(?P<type>.*)int:(?P<int>.*)",
		line,
	)

	return Integer_cst{
		VarType: strings.TrimSpace(groups["type"]),
		VarInt:  strings.TrimSpace(groups["int"]),
	}
}
