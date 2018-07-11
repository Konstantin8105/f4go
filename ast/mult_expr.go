package ast

import "strings"

type Mult_expr struct {
	Type string
	Op0  string
	Op1  string
}

func (a Mult_expr) GenNodeName() string {
	return "Mult_expr "
}

func parse_mult_expr(line string) (n Node) {
	groups := groupsFromRegex(
		`
	type:(?P<type>.*) +
	op 0:(?P<op0>.*) +
	op 1:(?P<op1>.*) +
	`,
		line,
	)
	return Mult_expr{
		Type: strings.TrimSpace(groups["type"]),
		Op0:  strings.TrimSpace(groups["op0"]),
		Op1:  strings.TrimSpace(groups["op1"]),
	}
}
