package ast

type Eq_expr struct {
	Op0  string
	Op1  string
	Type string
}

func (a Eq_expr) GenNodeName() string {
	return "eq_expr"
}
func parse_eq_expr(line string) (n Node) {
	return Eq_expr{
		Op0:  findVal("op 0:", &line),
		Op1:  findVal("op 1:", &line),
		Type: findVal("type:", &line),
	}
}
