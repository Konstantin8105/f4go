package ast

import "strings"

type Fix_trunc_expr struct {
	Type string
	Op0  string
}

func (a Fix_trunc_expr) GenNodeName() string {
	return "Fix_trunc_expr "
}

func parse_fix_trunc_expr(line string) (n Node) {
	groups := groupsFromRegex(
		`
	type:(?P<type>.*) +
	op 0:(?P<op0>.*) *
	`,
		line,
	)
	return Fix_trunc_expr{
		Type: strings.TrimSpace(groups["type"]),
		Op0:  strings.TrimSpace(groups["op0"]),
	}
}
