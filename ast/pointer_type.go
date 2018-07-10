package ast

import "strings"

type Pointer_type struct {
	Size string
	Algn string
	Ptd  string
}

func parse_pointer_type(line string) (n Node) {
	groups := groupsFromRegex(
		`
		size:(?P<size>.*)
		algn:(?P<algn>.*)
		ptd :(?P<ptd>.*)
		`,
		line,
	)

	return Pointer_type{
		Size: strings.TrimSpace(groups["size"]),
		Algn: strings.TrimSpace(groups["algn"]),
		Ptd:  strings.TrimSpace(groups["ptd"]),
	}
}
