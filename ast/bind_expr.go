package ast

import "strings"

type Bind_expr struct {
	TypeD string
	Vars  string
	Body  string
}

func parse_bind_expr(line string) (n interface{}) {
	groups := groupsFromRegex(
		`
		type:(?P<type>.*)
		vars:(?P<vars>.*)
		body:(?P<body>.*)
		`,
		line,
	)

	return Bind_expr{
		TypeD: strings.TrimSpace(groups["type"]),
		Vars:  strings.TrimSpace(groups["vars"]),
		Body:  strings.TrimSpace(groups["body"]),
	}
}
