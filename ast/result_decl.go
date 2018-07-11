package ast

import "strings"

type Result_decl struct {
	Type string
	Scpe string
	Srcp string
	Note string
	Size string
	Algn string
}

func (a Result_decl) GenNodeName() string {
	return "Result_decl "
}

func parse_result_decl(line string) (n Node) {
	groups := groupsFromRegex(
		`
	type:(?P<type>.*) +
	scpe:(?P<scpe>.*) +
	srcp:(?P<srcp>.*) +
	note:(?P<note>.*) +
	size:(?P<size>.*) +
	algn:(?P<algn>.*) +
	`,
		line,
	)
	return Result_decl{
		Type: strings.TrimSpace(groups["type"]),
		Scpe: strings.TrimSpace(groups["scpe"]),
		Srcp: strings.TrimSpace(groups["srcp"]),
		Note: strings.TrimSpace(groups["note"]),
		Size: strings.TrimSpace(groups["size"]),
		Algn: strings.TrimSpace(groups["algn"]),
	}
}
