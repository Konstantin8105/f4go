package ast

import (
	"strconv"
	"strings"
)

type Identifier_node struct {
	Lngt string
	Strg string
}

func (a Identifier_node) GenNodeName() string {
	return "identifier_node"
}
func parse_identifier_node(line string) (n Node) {
	s := Identifier_node{
		Lngt: findVal("lngt:", &line),
	}

	i, err := strconv.Atoi(s.Lngt)
	index := strings.Index(line, "strg:")
	if index < 0 || err != nil {
		s.Strg = findVal("strg:", &line)
		return s
	}

	s.Strg = line[index+6 : index+6+i]

	return s
}
