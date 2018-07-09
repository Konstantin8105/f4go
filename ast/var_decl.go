package ast

import "strings"

type var_decl struct {
	name  string
	typeD string
	scpe  string
	srcp  string
	note  string
	size  string
	used  string
}

func parse_var_decl(line string) (n interface{}) {
	groups := groupsFromRegex(
		`
		(name:(?P<name>.*))?
		type:(?P<type>.*)
		scpe:(?P<scpe>.*)
		srcp:(?P<srcp>.*)
		(note:(?P<note>.*))?
		size:(?P<size>.*)
		used:(?P<used>.*)
		`,
		line,
	)

	return var_decl{
		name:  strings.TrimSpace(groups["name"]),
		typeD: strings.TrimSpace(groups["type"]),
		scpe:  strings.TrimSpace(groups["scpe"]),
		srcp:  strings.TrimSpace(groups["srcp"]),
		note:  strings.TrimSpace(groups["note"]),
		size:  strings.TrimSpace(groups["size"]),
		used:  strings.TrimSpace(groups["used"]),
	}
}
