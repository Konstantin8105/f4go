package ast

import "strings"

type Const_decl struct {
	Type string
	Scpe string
	Srcp string
	Cnst string
}

func (a Const_decl) GenNodeName() string {
	return "Const_decl "
}

func parse_const_decl(line string) (n Node) {
	groups := groupsFromRegex(
		`
	type:(?P<type>.*) +
	scpe:(?P<scpe>.*) +
	srcp:(?P<srcp>.*) +
	cnst:(?P<cnst>.*) *
	`,
		line,
	)
	return Const_decl{
		Type: strings.TrimSpace(groups["type"]),
		Scpe: strings.TrimSpace(groups["scpe"]),
		Srcp: strings.TrimSpace(groups["srcp"]),
		Cnst: strings.TrimSpace(groups["cnst"]),
	}
}
