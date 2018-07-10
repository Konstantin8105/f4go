
package ast

import "strings"

type Real_cst struct {
	Type string
	Valu string
}

func parse_real_cst(line string) (n interface{}) {
	groups := groupsFromRegex(
	`
	type:(?P<type>.*) +
	valu:(?P<valu>.*) +
	`,
	line,
	)
	return Real_cst{
		Type: strings.TrimSpace(groups["type"]),
		Valu: strings.TrimSpace(groups["valu"]),
	}
}
