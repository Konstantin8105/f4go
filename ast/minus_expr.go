package ast

import (
	"strings"
)

type Minus_expr struct {
	Type string
	Op0  string
	Op1  string
}

func (a Minus_expr) GenNodeName() string {
	return "Minus_expr "
}

func parse_minus_expr(line string) (n Node) {
	groups := groupsFromRegex(
		`
	type:(?P<type>.*) +
	op 0:(?P<op0>.*) +
	op 1:(?P<op1>.*) *
	`,
		line,
	)
	return Minus_expr{
		Type: strings.TrimSpace(groups["type"]),
		Op0:  strings.TrimSpace(groups["op0"]),
		Op1:  strings.TrimSpace(groups["op1"]),
	}
}
