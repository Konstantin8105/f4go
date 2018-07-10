package ast

import "strings"

type Pointer_plus_expr struct {
	Type string
	Op0  string
	Op1  string
}

func parse_pointer_plus_expr(line string) (n interface{}) {
	groups := groupsFromRegex(
		`
	type:(?P<type>.*) +
	op 0:(?P<op0>.*) +
	op 1:(?P<op1>.*) +
	`,
		line,
	)
	return Pointer_plus_expr{
		Type: strings.TrimSpace(groups["type"]),
		Op0:  strings.TrimSpace(groups["op0"]),
		Op1:  strings.TrimSpace(groups["op1"]),
	}
}
