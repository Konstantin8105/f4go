package ast

type Identifier_node struct {
	Lngt string
	Strg string
}

func (a Identifier_node) GenNodeName() string {
	return "identifier_node"
}
func parse_identifier_node(line string) (n Node) {
	return Identifier_node{
		Lngt: findVal("lngt:", &line),
		Strg: findVal("strg:", &line),
	}
}
