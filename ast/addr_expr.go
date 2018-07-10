package ast

import "strings"

type Addr_expr struct {
	Type string
	Op0  string
}

func parse_addr_expr(line string) (n Node) {
	groups := groupsFromRegex(
		`
	type:(?P<type>.*) +
	op 0:(?P<op0>.*) +
	`,
		line,
	)
	return Addr_expr{
		Type: strings.TrimSpace(groups["type"]),
		Op0:  strings.TrimSpace(groups["op0"]),
	}
}
