package ast

import (
	"fmt"
	"strings"
)

type Call_expr struct {
	Type string
	Fn   string
	Vals []string
}

func (a Call_expr) GenNodeName() string {
	return "Call_expr "
}

func parse_call_expr(line string) (n Node) {

	var c Call_expr
	var index int

	if index = strings.Index(line, "type:"); index > 0 {
		begin := index + 6
		for i := begin; i < len(line); i++ {
			if line[i] == ' ' {
				c.Type = line[begin:i]
				break
			}
		}
	}

	if index = strings.Index(line, "fn  :"); index > 0 {
		begin := index + 6
		for i := begin; i < len(line); i++ {
			if line[i] == ' ' {
				c.Fn = line[begin:i]
				break
			}
		}
	}

	for counter := 0; ; counter++ {
		var name string
		if counter < 10 {
			name = fmt.Sprintf("%d   :", counter)
		} else {
			name = fmt.Sprintf("%d  :", counter)
		}

		if index = strings.Index(line, name); index > 0 {
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
		} else {
			break
		}
	}

	return c
}
