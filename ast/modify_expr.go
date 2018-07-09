package ast

import "strings"

type modify_expr struct {
	typeD string
	op0   string
	op1   string
}

func parse_modify_expr(line string) (n interface{}) {
	groups := groupsFromRegex(
		`
		type:(?P<type>.*)
		op 0:(?P<op0>.*)
		op 1:(?P<op1>.*)
		`,
		line,
	)

	return modify_expr{
		typeD: strings.TrimSpace(groups["type"]),
		op0:   strings.TrimSpace(groups["op0"]),
		op1:   strings.TrimSpace(groups["op1"]),
	}
}
