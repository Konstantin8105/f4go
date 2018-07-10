package ast

import (
	"strings"
)

type Record_type struct {
	Name string
	Size string
	Algn string
	Tag  string
	Flds string
}

func parse_record_type(line string) (n interface{}) {
	groups := groupsFromRegex(
		`
	name:(?P<name>.*) +
	size:(?P<size>.*) +
	algn:(?P<algn>.*) +
	tag :(?P<tag>.*) +
	flds:(?P<flds>.*) +
	`,
		line,
	)
	return Record_type{
		Name: strings.TrimSpace(groups["name"]),
		Size: strings.TrimSpace(groups["size"]),
		Algn: strings.TrimSpace(groups["algn"]),
		Tag:  strings.TrimSpace(groups["tag"]),
		Flds: strings.TrimSpace(groups["flds"]),
	}
}
