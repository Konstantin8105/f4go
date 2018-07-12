package ast

type Pointer_plus_expr struct {
	Op0  string
	Op1  string
	Type string
}

func (a Pointer_plus_expr) GenNodeName() string {
	return "pointer_plus_expr"
}
func parse_pointer_plus_expr(line string) (n Node) {
	return Pointer_plus_expr{
		Op0:  findVal("op 0:", &line),
		Op1:  findVal("op 1:", &line),
		Type: findVal("type:", &line),
	}
}
