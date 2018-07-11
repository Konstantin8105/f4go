package ast

import "strings"

type Function_type struct {
	Size string
	Algn string
	Retn string
	Prms string
}

func (a Function_type) GenNodeName() string {
	return "Function_type "
}

func parse_function_type(line string) (n Node) {
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
