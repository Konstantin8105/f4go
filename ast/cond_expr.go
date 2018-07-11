package ast

import (
	"strings"
)

type Cond_expr struct {
	Type string
	Op0  string
	Op1  string
	Op2  string
}

func (a Cond_expr) GenNodeName() string {
	return "Cond_expr "
}

func parse_cond_expr(line string) (n Node) {
	groups := groupsFromRegex(
		`
	type:(?P<type>.*) +
	op 0:(?P<op0>.*) +
	op 1:(?P<op1>.*) +
	op 2:(?P<op2>.*) *
	`,
		line,
	)

	return Cond_expr{
		Type: strings.TrimSpace(groups["type"]),
		Op0:  strings.TrimSpace(groups["op0"]),
		Op1:  strings.TrimSpace(groups["op1"]),
		Op2:  strings.TrimSpace(groups["op2"]),
	}
}
