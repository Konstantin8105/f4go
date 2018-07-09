package ast

import "strings"

type bind_expr struct {
	typeD string
	vars  string
	body  string
}

func parse_bind_expr(line string) (n interface{}) {
	groups := groupsFromRegex(
		`
		type:(?P<type>.*)
		(vars:(?P<vars>.*))?
		body:(?P<body>.*)
		`,
		line,
	)

	return bind_expr{
		typeD: strings.TrimSpace(groups["type"]),
		vars:  strings.TrimSpace(groups["vars"]),
		body:  strings.TrimSpace(groups["body"]),
	}
}
