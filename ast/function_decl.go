package ast

import (
	"strings"
)

type Function_decl struct {
	Name     string
	Mngl     string
	VarType  string
	Scpe     string
	Srcp     string
	Args     string
	IsStatic bool
	IsExtern bool
	Body     string
}

func parse_function_decl(line string) (n interface{}) {
	groups := groupsFromRegex(
		`
		name:(?P<name> @[0-9]+) +
		(mngl:(?P<mngl> @[0-9]+) +)?
		type:(?P<type> @[0-9]+)
		 +
		(scpe:(?P<scpe> @[0-9]+) +)?
		(srcp:(?P<srcp>.*:[0-9]+) +)?
		(args:(?P<args> @[0-9]+) +)?
		(?P<static>link: static)?
		(?P<extern>link: extern)?
		( +)?
		(body:(?P<body>.*))?
		( +)?
		(?P<static>link: static)?
		(?P<extern>link: extern)?
		`,
		line,
	)

	return Function_decl{
		Name:     strings.TrimSpace(groups["name"]),
		Mngl:     strings.TrimSpace(groups["mngl"]),
		VarType:  strings.TrimSpace(groups["type"]),
		Scpe:     strings.TrimSpace(groups["scpe"]),
		Srcp:     strings.TrimSpace(groups["srcp"]),
		Args:     strings.TrimSpace(groups["args"]),
		IsStatic: len(groups["static"]) > 0,
		IsExtern: len(groups["extern"]) > 0,
		Body:     strings.TrimSpace(groups["body"]),
	}
}
