package ast

type Trunc_div_expr struct {
	Op0  string
	Op1  string
	Type string
}

func (a Trunc_div_expr) GenNodeName() string {
	return "trunc_div_expr"
}
func parse_trunc_div_expr(line string) (n Node) {
	return Trunc_div_expr{
		Op0:  findVal("op 0:", &line),
		Op1:  findVal("op 1:", &line),
		Type: findVal("type:", &line),
	}
}
