package ast

import "strings"

type Bind_expr struct {
	TypeD string
	Vars  string
	Body  string
}

func (a Bind_expr) GenNodeName() string {
	return "Bind_expr "
}

func parse_bind_expr(line string) (n Node) {
	groups := groupsFromRegex(
		`
		type:(?P<type> @[0-9]+) +
		(vars:(?P<vars>.*) +)?
		body:(?P<body>.*) *
		`,
		line,
	)

	return Bind_expr{
		TypeD: strings.TrimSpace(groups["type"]),
		Vars:  strings.TrimSpace(groups["vars"]),
		Body:  strings.TrimSpace(groups["body"]),
	}
}
