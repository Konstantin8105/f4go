package ast

import (
	"strings"
)

type Statement_list struct {
	Vals []string
}

func parse_statement_list(line string) (n Node) {
	var c Statement_list
	var index int
	for {
		indexNew := strings.Index(line[index:], ":")
		if indexNew < 0 {
			break
		}
		index += indexNew + 1
		begin := index + 1
		for i := begin; i < len(line); i++ {
			if line[i] == ' ' {
				c.Vals = append(c.Vals, line[begin:i])
				break
			}
			if i == len(line)-1 {
				c.Vals = append(c.Vals, line[begin:])
				break
			}
		}
	}

	return c
}
