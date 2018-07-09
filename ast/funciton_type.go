package ast

import "strings"

type Function_type struct {
	Size string
	Algn string
	Retn string
	Prms string
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

	return Function_type{
		Size: strings.TrimSpace(groups["size"]),
		Algn: strings.TrimSpace(groups["algn"]),
		Retn: strings.TrimSpace(groups["retn"]),
		Prms: strings.TrimSpace(groups["prms"]),
	}
}
