package ast

import "strings"

type Loop_expr struct {
	Type string
	Body string
}

func parse_loop_expr(line string) (n interface{}) {
	groups := groupsFromRegex(
		`
	type:(?P<type>.*) +
	body:(?P<body>.*) +
	`,
		line,
	)
	return Loop_expr{
		Type: strings.TrimSpace(groups["type"]),
		Body: strings.TrimSpace(groups["body"]),
	}
}
