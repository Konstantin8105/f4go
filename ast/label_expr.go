package ast

import "strings"

type Label_expr struct {
	Type string
	Name string
}

func (a Label_expr) GenNodeName() string {
	return "Label_expr "
}

func parse_label_expr(line string) (n Node) {
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
