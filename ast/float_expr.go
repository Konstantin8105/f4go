package ast

import "strings"

type Float_expr struct {
	Type string
	Op0  string
}

func parse_float_expr(line string) (n interface{}) {
	groups := groupsFromRegex(
		`
	type:(?P<type>.*) +
	op 0:(?P<op0>.*) +
	`,
		line,
	)
	return Float_expr{
		Type: strings.TrimSpace(groups["type"]),
		Op0:  strings.TrimSpace(groups["op0"]),
	}
}