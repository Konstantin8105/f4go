package ast

import (
	"strings"
)

type identifier_node struct {
	link string
	strg string
	lngt string
}

func parse_identifier_node(line string) (n interface{}) {
	groups := groupsFromRegex(
		"strg:(?P<strg>.*)lngt:(?P<lngt>.*)",
		line,
	)

	return identifier_node{
		strg: strings.TrimSpace(groups["strg"]),
		lngt: strings.TrimSpace(groups["lngt"]),
	}
}
