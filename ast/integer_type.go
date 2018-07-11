package ast

import "strings"

type Integer_type struct {
	Name string
	Size string
	Algn string
	Prec string
	Sign string
	Min  string
	Max  string
}

func (a Integer_type) GenNodeName() string {
	return "Integer_type "
}

func parse_integer_type(line string) (n Node) {
	groups := groupsFromRegex(
		`
		(name:(?P<name>.*))?
		size:(?P<size>.*)
		algn:(?P<algn>.*)
		prec:(?P<prec>.*)
		sign:(?P<sign>.*)
		min :(?P<min>.*)
		max :(?P<max>.*)
		`,
		line,
	)

	return Integer_type{
		Name: strings.TrimSpace(groups["name"]),
		Size: strings.TrimSpace(groups["size"]),
		Algn: strings.TrimSpace(groups["algn"]),
		Prec: strings.TrimSpace(groups["prec"]),
		Sign: strings.TrimSpace(groups["sign"]),
		Min:  strings.TrimSpace(groups["min"]),
		Max:  strings.TrimSpace(groups["max"]),
	}
}
