package ast

import "strings"

type Reference_type struct {
	Size string
	Algn string
	Refd string
}

func parse_reference_type(line string) (n Node) {
	groups := groupsFromRegex(
		`
	size:(?P<size>.*) +
	algn:(?P<algn>.*) +
	refd:(?P<refd>.*) +
	`,
		line,
	)
	return Reference_type{
		Size: strings.TrimSpace(groups["size"]),
		Algn: strings.TrimSpace(groups["algn"]),
		Refd: strings.TrimSpace(groups["refd"]),
	}
}
