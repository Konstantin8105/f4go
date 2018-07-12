package ast

type Integer_cst struct {
	Int  string
	Type string
}

func (a Integer_cst) GenNodeName() string {
	return "integer_cst"
}
func parse_integer_cst(line string) (n Node) {
	return Integer_cst{
		Int:  findVal(" int:", &line),
		Type: findVal("type:", &line),
	}
}
