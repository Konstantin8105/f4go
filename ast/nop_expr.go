package ast

import "strings"

type Nop_expr struct {
	Type string
	Op0  string
}

func parse_nop_expr(line string) (n Node) {
	groups := groupsFromRegex(
		`
	type:(?P<type>.*) +
	op 0:(?P<op0>.*) +
	`,
		line,
	)
	return Nop_expr{
		Type: strings.TrimSpace(groups["type"]),
		Op0:  strings.TrimSpace(groups["op0"]),
	}
}
