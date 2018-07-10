package ast

import "strings"

type Array_type struct {
	Size string
	Algn string
	Elts string
	Domn string
}

func parse_array_type(line string) (n interface{}) {
	groups := groupsFromRegex(
		`
	size:(?P<size>.*) +
	algn:(?P<algn>.*) +
	elts:(?P<elts>.*) +
	domn:(?P<domn>.*) +
	`,
		line,
	)
	return Array_type{
		Size: strings.TrimSpace(groups["size"]),
		Algn: strings.TrimSpace(groups["algn"]),
		Elts: strings.TrimSpace(groups["elts"]),
		Domn: strings.TrimSpace(groups["domn"]),
	}
}
