package ast

import "strings"

type Field_decl struct {
	Name string
	Type string
	Scpe string
	Size string
	Algn string
	Bpos string
}

func parse_field_decl(line string) (n interface{}) {
	groups := groupsFromRegex(
		`
	name:(?P<name>.*) +
	type:(?P<type>.*) +
	scpe:(?P<scpe>.*) +
	size:(?P<size>.*) +
	algn:(?P<algn>.*) +
	bpos:(?P<bpos>.*) +
	`,
		line,
	)
	return Field_decl{
		Name: strings.TrimSpace(groups["name"]),
		Type: strings.TrimSpace(groups["type"]),
		Scpe: strings.TrimSpace(groups["scpe"]),
		Size: strings.TrimSpace(groups["size"]),
		Algn: strings.TrimSpace(groups["algn"]),
		Bpos: strings.TrimSpace(groups["bpos"]),
	}
}