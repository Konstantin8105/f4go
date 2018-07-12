package ast

type Plus_expr struct {
	Op0  string
	Op1  string
	Type string
}

func (a Plus_expr) GenNodeName() string {
	return "plus_expr"
}
func parse_plus_expr(line string) (n Node) {
	return Plus_expr{
		Op0:  findVal("op 0:", &line),
		Op1:  findVal("op 1:", &line),
		Type: findVal("type:", &line),
	}
}
