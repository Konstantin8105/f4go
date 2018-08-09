package fortran

import "go/token"

// Examples :
// ( 1, 2)
// ( *, N*2)
// ( 123, func(2,3))
func separateArgsParen(nodes []node) (args [][]node, end int) {
	if nodes[0].tok != token.LPAREN {
		panic("First symbol is not '(' : " + nodesToString(nodes))
	}
	var counter int
	args = append(args, []node{})
	for end = 0; end < len(nodes); end++ {
		if nodes[end].tok == token.LPAREN {
			counter++
			continue
		}
		if nodes[end].tok == token.COMMA && counter == 1 {
			args = append(args, []node{})
			continue
		}
		if nodes[end].tok == token.RPAREN {
			counter--
			if counter == 0 {
				break
			}
			continue
		}
		args[len(args)-1] = append(args[len(args)-1], nodes[end])
	}
	if nodes[end].tok != token.RPAREN {
		panic("Last symbol is not ')' : " + nodesToString(nodes))
	}
	end++
	return
}
