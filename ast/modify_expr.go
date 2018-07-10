package ast

import "strings"

type Modify_expr struct {
	TypeD string
	Op0   string
	Op1   string
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

	return Modify_expr{
		TypeD: strings.TrimSpace(groups["type"]),
		Op0:   strings.TrimSpace(groups["op0"]),
		Op1:   strings.TrimSpace(groups["op1"]),
	}
}