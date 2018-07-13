package ast

import (
	"strconv"
	"strings"
)

type String_cst struct {
	Lngt string
	Strg string
	Type string
}

func (a String_cst) GenNodeName() string {
	return "string_cst"
}
func parse_string_cst(line string) (n Node) {
	s := String_cst{
		Lngt: findVal("lngt:", &line),
		Type: findVal("type:", &line),
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
