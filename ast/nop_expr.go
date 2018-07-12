package ast

type Nop_expr struct {
	Op0  string
	Type string
}

func (a Nop_expr) GenNodeName() string {
	return "nop_expr"
}
func parse_nop_expr(line string) (n Node) {
	return Nop_expr{
		Op0:  findVal("op 0:", &line),
		Type: findVal("type:", &line),
	}
}
