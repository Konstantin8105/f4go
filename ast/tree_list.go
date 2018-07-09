package ast

import "strings"

type Tree_list struct {
	Valu string
}

func parse_tree_list(line string) (n interface{}) {
	groups := groupsFromRegex(
		`
		valu:(?P<valu>.*)
		`,
		line,
	)

	return Tree_list{
		Valu: strings.TrimSpace(groups["valu"]),
	}
}
