package ast

type Mult_expr struct {
	Op0  string
	Op1  string
	Type string
}

func (a Mult_expr) GenNodeName() string {
	return "mult_expr"
}
func parse_mult_expr(line string) (n Node) {
	return Mult_expr{
		Op0:  findVal("op 0:", &line),
		Op1:  findVal("op 1:", &line),
		Type: findVal("type:", &line),
	}
}
