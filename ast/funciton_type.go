package ast

import "strings"

type function_type struct {
	size string
	algn string
	retn string
	prms string
}

func parse_function_type(line string) (n interface{}) {
	groups := groupsFromRegex(
		`
		size:(?P<size>.*)
		algn:(?P<algn>.*)
		retn:(?P<retn>.*)
		prms:(?P<prms>.*)
		`,
		line,
	)

	return function_type{
		size: strings.TrimSpace(groups["size"]),
		algn: strings.TrimSpace(groups["algn"]),
		retn: strings.TrimSpace(groups["retn"]),
		prms: strings.TrimSpace(groups["prms"]),
	}
}
