package ast

import "strings"

type Goto_expr struct {
	Type string
	Labl string
}

func parse_goto_expr(line string) (n interface{}) {
	groups := groupsFromRegex(
		`
	type:(?P<type>.*) +
	labl:(?P<labl>.*) +
	`,
		line,
	)
	return Goto_expr{
		Type: strings.TrimSpace(groups["type"]),
		Labl: strings.TrimSpace(groups["labl"]),
	}
}
