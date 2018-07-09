package ast

import "strings"

type integer_type struct {
	name string
	size string
	algn string
	prec string
	sign string
	min  string
	max  string
}

func parse_integer_type(line string) (n interface{}) {
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

	return integer_type{
		name: strings.TrimSpace(groups["name"]),
		size: strings.TrimSpace(groups["size"]),
		algn: strings.TrimSpace(groups["algn"]),
		prec: strings.TrimSpace(groups["prec"]),
		sign: strings.TrimSpace(groups["sign"]),
		min:  strings.TrimSpace(groups["min"]),
		max:  strings.TrimSpace(groups["max"]),
	}
}
