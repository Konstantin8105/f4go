package ast

import (
	"strings"
)

type Identifier_node struct {
	Link string
	Strg string
	Lngt string
}

func (a Identifier_node) GenNodeName() string {
	return "Identifier_node "
}

func parse_identifier_node(line string) (n Node) {
	groups := groupsFromRegex(
		"strg:(?P<strg>.*)lngt:(?P<lngt>.*)",
		line,
	)

	return Identifier_node{
		Strg: strings.TrimSpace(groups["strg"]),
		Lngt: strings.TrimSpace(groups["lngt"]),
	}
}
