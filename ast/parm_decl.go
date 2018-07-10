package ast

import "strings"

type Parm_decl struct {
	Name string
	Type string
	Scpe string
	Srcp string
	Argt string
	Size string
	Algn string
	Used string
}

func parse_parm_decl(line string) (n interface{}) {
	groups := groupsFromRegex(
		`
	name:(?P<name>.*) +
	type:(?P<type>.*) +
	scpe:(?P<scpe>.*) +
	srcp:(?P<srcp>.*) +
	argt:(?P<argt>.*) +
	size:(?P<size>.*) +
	algn:(?P<algn>.*) +
	used:(?P<used>.*) +
	`,
		line,
	)
	return Parm_decl{
		Name: strings.TrimSpace(groups["name"]),
		Type: strings.TrimSpace(groups["type"]),
		Scpe: strings.TrimSpace(groups["scpe"]),
		Srcp: strings.TrimSpace(groups["srcp"]),
		Argt: strings.TrimSpace(groups["argt"]),
		Size: strings.TrimSpace(groups["size"]),
		Algn: strings.TrimSpace(groups["algn"]),
		Used: strings.TrimSpace(groups["used"]),
	}
}
