package ast

import "strings"

type Le_expr struct {
	Type string
	Op0  string
	Op1  string
}

func (a Le_expr) GenNodeName() string {
	return "Le_expr "
}

func parse_le_expr(line string) (n Node) {
	groups := groupsFromRegex(
		`
	type:(?P<type>.*) +
	op 0:(?P<op0>.*) +
	op 1:(?P<op1>.*) +
	`,
		line,
	)
	return Le_expr{
		Type: strings.TrimSpace(groups["type"]),
		Op0:  strings.TrimSpace(groups["op0"]),
		Op1:  strings.TrimSpace(groups["op1"]),
	}
}
