package ast

import "strings"

type Type_decl struct {
	Name    string
	VarType string
	Srcp    string
}

func (a Type_decl) GenNodeName() string {
	return "Type_decl "
}

func parse_type_decl(line string) (n Node) {
	groups := groupsFromRegex(
		"name:(?P<name>.*)type:(?P<type>.*)srcp:(?P<srcp>.*)",
		line,
	)

	return Type_decl{
		Name:    strings.TrimSpace(groups["name"]),
		VarType: strings.TrimSpace(groups["type"]),
		Srcp:    strings.TrimSpace(groups["srcp"]),
	}
}
