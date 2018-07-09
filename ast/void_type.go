package ast

import "strings"

type void_type struct {
	name string
	qual string
	unql string
	algn string
}

func parse_void_type(line string) (n interface{}) {
	groups := groupsFromRegex(
		`
		(name:(?P<name>.*))?
		(qual:(?P<qual>.*))?
		(unql:(?P<unql>.*))?
		algn:(?P<algn>.*)
		`,
		line,
	)

	return void_type{
		name: strings.TrimSpace(groups["name"]),
		qual: strings.TrimSpace(groups["qual"]),
		unql: strings.TrimSpace(groups["unql"]),
		algn: strings.TrimSpace(groups["algn"]),
	}
}
