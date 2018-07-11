package ast

import "strings"

type Tree_list struct {
	Valu string
}

func (a Tree_list) GenNodeName() string {
	return "Tree_list "
}

func parse_tree_list(line string) (n Node) {
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
