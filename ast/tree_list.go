package ast

import "strings"

type Tree_list struct {
	Valu string
	Chan string
}

func (a Tree_list) GenNodeName() string {
	return "Tree_list "
}

func parse_tree_list(line string) (n Node) {
	groups := groupsFromRegex(
		`
		valu:(?P<valu> @[0-9]+) +
		(chan: (?P<chan>.*))?
		`,
		line,
	)

	return Tree_list{
		Valu: strings.TrimSpace(groups["valu"]),
		Chan: strings.TrimSpace(groups["chan"]),
	}
}
