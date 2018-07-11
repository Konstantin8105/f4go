package ast

import (
	"strings"
)

type Var_decl struct {
	Name  string
	TypeD string
	Scpe  string
	Srcp  string
	Note  string
	Size  string
	Used  string
}

func (a Var_decl) GenNodeName() string {
	return "Var_decl "
}

func parse_var_decl(line string) (n Node) {
	groups := groupsFromRegex(
		`
		(name: (?P<name>.*))?
		type: (?P<type>.*) +
		scpe: (?P<scpe>.*) +
		srcp: (?P<srcp>.*) +
		(note: (?P<note>.*) +)?
		size: (?P<size>.*) +
		used: (?P<used>.*)
		`,
		line,
	)

	return Var_decl{
		Name:  strings.TrimSpace(groups["name"]),
		TypeD: strings.TrimSpace(groups["type"]),
		Scpe:  strings.TrimSpace(groups["scpe"]),
		Srcp:  strings.TrimSpace(groups["srcp"]),
		Note:  strings.TrimSpace(groups["note"]),
		Size:  strings.TrimSpace(groups["size"]),
		Used:  strings.TrimSpace(groups["used"]),
	}
}
