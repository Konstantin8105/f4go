package ast

import (
	"strings"
)

type Constructor struct {
	Lngt string
	Vals []string
}

func parse_constructor(line string) (n Node) {
	var c Constructor
	var index int

	if index = strings.Index(line, "lngt:"); index > 0 {
		begin := index + 6
		for i := begin; i < len(line); i++ {
			if line[i] == ' ' {
				c.Lngt = line[begin:i]
				break
			}
		}
	}

	for {
		indexNew := strings.Index(line[index:], "val :")
		if indexNew < 0 {
			break
		}
		index += indexNew + 1
		begin := index + 6
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
