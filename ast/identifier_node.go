package ast

import (
	"strings"
)

type Identifier_node struct {
	Link string
	Strg string
	Lngt string
}

func parse_identifier_node(line string) (n interface{}) {
	groups := groupsFromRegex(
		"strg:(?P<strg>.*)lngt:(?P<lngt>.*)",
		line,
	)

	return Identifier_node{
		Strg: strings.TrimSpace(groups["strg"]),
		Lngt: strings.TrimSpace(groups["lngt"]),
	}
}
