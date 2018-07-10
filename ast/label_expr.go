package ast

import "strings"

type Label_expr struct {
	Type string
	Name string
}

func parse_label_expr(line string) (n interface{}) {
	groups := groupsFromRegex(
		`
	type:(?P<type>.*) +
	name:(?P<name>.*) +
	`,
		line,
	)
	return Label_expr{
		Type: strings.TrimSpace(groups["type"]),
		Name: strings.TrimSpace(groups["name"]),
	}
}
