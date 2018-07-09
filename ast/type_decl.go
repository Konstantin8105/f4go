package ast

import "strings"

type type_decl struct {
	name    string
	varType string
	srcp    string
}

func parse_type_decl(line string) (n interface{}) {
	groups := groupsFromRegex(
		"name:(?P<name>.*)type:(?P<type>.*)srcp:(?P<srcp>.*)",
		line,
	)

	return type_decl{
		name:    strings.TrimSpace(groups["name"]),
		varType: strings.TrimSpace(groups["type"]),
		srcp:    strings.TrimSpace(groups["srcp"]),
	}
}
