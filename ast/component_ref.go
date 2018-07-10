package ast

import "strings"

type Component_ref struct {
	Type string
	Op0  string
	Op1  string
}

func parse_component_ref(line string) (n interface{}) {
	groups := groupsFromRegex(
		`
	type:(?P<type>.*) +
	op 0:(?P<op0>.*) +
	op 1:(?P<op1>.*) +
	`,
		line,
	)
	return Component_ref{
		Type: strings.TrimSpace(groups["type"]),
		Op0:  strings.TrimSpace(groups["op0"]),
		Op1:  strings.TrimSpace(groups["op1"]),
	}
}
