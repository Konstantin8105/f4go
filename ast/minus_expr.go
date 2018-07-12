package ast

type Minus_expr struct {
	Op0  string
	Op1  string
	Type string
}

func (a Minus_expr) GenNodeName() string {
	return "minus_expr"
}
func parse_minus_expr(line string) (n Node) {
	return Minus_expr{
		Op0:  findVal("op 0:", &line),
		Op1:  findVal("op 1:", &line),
		Type: findVal("type:", &line),
	}
}
