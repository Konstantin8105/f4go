package ast

import "strings"

type function_decl struct {
	name    string
	mngl    string
	varType string
	scpe    string
	srcp    string
	link    string
	body    string
}

func parse_function_decl(line string) (n interface{}) {
	groups := groupsFromRegex(
		`
		name:(?P<name>.*)
		(mngl:(?P<mngl>.*))?
		type:(?P<type>.*)
		(scpe:(?P<scpe>.*))?
		(srcp:(?P<srcp>.*))?
		(link:(?P<link>.*)|body:(?P<body>.*))?
		(body:(?P<body>.*)|link:(?P<link>.*))?
		`,
		line,
	)

	return function_decl{
		name:    strings.TrimSpace(groups["name"]),
		mngl:    strings.TrimSpace(groups["mngl"]),
		varType: strings.TrimSpace(groups["type"]),
		scpe:    strings.TrimSpace(groups["scpe"]),
		srcp:    strings.TrimSpace(groups["srcp"]),
		link:    strings.TrimSpace(groups["link"]),
		body:    strings.TrimSpace(groups["body"]),
	}
}
