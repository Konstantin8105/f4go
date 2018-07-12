package ast

type Le_expr struct {
	Op0  string
	Op1  string
	Type string
}

func (a Le_expr) GenNodeName() string {
	return "le_expr"
}
func parse_le_expr(line string) (n Node) {
	return Le_expr{
		Op0:  findVal("op 0:", &line),
		Op1:  findVal("op 1:", &line),
		Type: findVal("type:", &line),
	}
}
