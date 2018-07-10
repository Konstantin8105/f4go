package ast

import "strings"

type Void_type struct {
	Name string
	Qual string
	Unql string
	Algn string
}

func parse_void_type(line string) (n Node) {
	groups := groupsFromRegex(
		`
		(name:(?P<name>.*))?
		(qual:(?P<qual>.*))?
		(unql:(?P<unql>.*))?
		algn:(?P<algn>.*)
		`,
		line,
	)

	return Void_type{
		Name: strings.TrimSpace(groups["name"]),
		Qual: strings.TrimSpace(groups["qual"]),
		Unql: strings.TrimSpace(groups["unql"]),
		Algn: strings.TrimSpace(groups["algn"]),
	}
}
