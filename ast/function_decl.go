package ast

import "strings"

type Function_decl struct {
	Name    string
	Mngl    string
	VarType string
	Scpe    string
	Srcp    string
	Link    string
	Body    string
}

func parse_function_decl(line string) (n interface{}) {
	groups := groupsFromRegex(
		`
		name:(?P<name>.*)
		mngl:(?P<mngl>.*)
		type:(?P<type>.*)
		scpe:(?P<scpe>.*)
		srcp:(?P<srcp>.*)
		link:(?P<link>.*)
		body:(?P<body>.*)
		`,
		// (body:(?P<body>.*)||link:(?P<link>.*))?
		line,
	)

	return Function_decl{
		Name:    strings.TrimSpace(groups["name"]),
		Mngl:    strings.TrimSpace(groups["mngl"]),
		VarType: strings.TrimSpace(groups["type"]),
		Scpe:    strings.TrimSpace(groups["scpe"]),
		Srcp:    strings.TrimSpace(groups["srcp"]),
		Link:    strings.TrimSpace(groups["link"]),
		Body:    strings.TrimSpace(groups["body"]),
	}
}
