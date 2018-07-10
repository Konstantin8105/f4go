package ast

import "strings"

type Indirect_ref struct {
	Type string
	Op0  string
}

func parse_indirect_ref(line string) (n interface{}) {
	groups := groupsFromRegex(
		`
	type:(?P<type>.*) +
	op 0:(?P<op0>.*) +
	`,
		line,
	)
	return Indirect_ref{
		Type: strings.TrimSpace(groups["type"]),
		Op0:  strings.TrimSpace(groups["op0"]),
	}
}