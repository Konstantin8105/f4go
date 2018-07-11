package ast

import "strings"

type Label_decl struct {
	Name string
	Type string
	Scpe string
	Srcp string
	Note string
}

func (a Label_decl) GenNodeName() string {
	return "Label_decl "
}

func parse_label_decl(line string) (n Node) {
	groups := groupsFromRegex(
		`
	name:(?P<name>.*) +
	type:(?P<type>.*) +
	scpe:(?P<scpe>.*) +
	srcp:(?P<srcp>.*) +
	note:(?P<note>.*) *
	`,
		line,
	)
	return Label_decl{
		Name: strings.TrimSpace(groups["name"]),
		Type: strings.TrimSpace(groups["type"]),
		Scpe: strings.TrimSpace(groups["scpe"]),
		Srcp: strings.TrimSpace(groups["srcp"]),
		Note: strings.TrimSpace(groups["note"]),
	}
}
