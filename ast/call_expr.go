package ast

import "strings"

type Call_expr struct {
	Type string
	Fn   string
}

func parse_call_expr(line string) (n Node) {
	groups := groupsFromRegex(
		`
	type:(?P<type>.*) +
	fn  :(?P<fn>.*) +
	`,
		line,
	)
	return Call_expr{
		Type: strings.TrimSpace(groups["type"]),
		Fn:   strings.TrimSpace(groups["fn"]),
	}
}
