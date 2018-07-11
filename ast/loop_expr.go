package ast

import "strings"

type Loop_expr struct {
	Type string
	Body string
}

func (a Loop_expr) GenNodeName() string {
	return "Loop_expr"
}

func parse_loop_expr(line string) (n Node) {
	groups := groupsFromRegex(
		`
	type:(?P<type>.*) +
	body:(?P<body>.*) *
	`,
		line,
	)
	return Loop_expr{
		Type: strings.TrimSpace(groups["type"]),
		Body: strings.TrimSpace(groups["body"]),
	}
}
