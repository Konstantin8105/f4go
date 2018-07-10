package ast

import "strings"

type Boolean_type struct {
	Name string
	Size string
	Algn string
}

func parse_boolean_type(line string) (n interface{}) {
	groups := groupsFromRegex(
		`
	name:(?P<name>.*) +
	size:(?P<size>.*) +
	algn:(?P<algn>.*) +
	`,
		line,
	)
	return Boolean_type{
		Name: strings.TrimSpace(groups["name"]),
		Size: strings.TrimSpace(groups["size"]),
		Algn: strings.TrimSpace(groups["algn"]),
	}
}
