package ast

type Cond_expr struct {
	Op0  string
	Op1  string
	Op2  string
	Type string
}

func (a Cond_expr) GenNodeName() string {
	return "cond_expr"
}
func parse_cond_expr(line string) (n Node) {
	return Cond_expr{
		Op0:  findVal("op 0:", &line),
		Op1:  findVal("op 1:", &line),
		Op2:  findVal("op 2:", &line),
		Type: findVal("type:", &line),
	}
}
