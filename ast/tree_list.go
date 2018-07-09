package ast

import "strings"

type tree_list struct {
	valu string
}

func parse_tree_list(line string) (n interface{}) {
	groups := groupsFromRegex(
		`
		valu:(?P<valu>.*)
		`,
		line,
	)

	return tree_list{
		valu: strings.TrimSpace(groups["valu"]),
	}
}
