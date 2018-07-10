package ast

import "strings"

type Real_type struct {
	Name string
	Size string
	Algn string
	Prec string
}

func parse_real_type(line string) (n Node) {
	groups := groupsFromRegex(
		`
	name:(?P<name>.*) +
	size:(?P<size>.*) +
	algn:(?P<algn>.*) +
	prec:(?P<prec>.*) +
	`,
		line,
	)
	return Real_type{
		Name: strings.TrimSpace(groups["name"]),
		Size: strings.TrimSpace(groups["size"]),
		Algn: strings.TrimSpace(groups["algn"]),
		Prec: strings.TrimSpace(groups["prec"]),
	}
}
