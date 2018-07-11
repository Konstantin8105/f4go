package ast

import "strings"

type Eq_expr struct {
	Type string
	Op0  string
	Op1  string
}

func (a Eq_expr) GenNodeName() string {
	return "Eq_expr "
}

func parse_eq_expr(line string) (n Node) {
	groups := groupsFromRegex(
		`
	type:(?P<type>.*) +
	op 0:(?P<op0>.*) +
	op 1:(?P<op1>.*) +
	`,
		line,
	)
	return Eq_expr{
		Type: strings.TrimSpace(groups["type"]),
		Op0:  strings.TrimSpace(groups["op0"]),
		Op1:  strings.TrimSpace(groups["op1"]),
	}
}
