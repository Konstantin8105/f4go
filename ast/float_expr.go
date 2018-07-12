package ast

type Float_expr struct {
	Op0  string
	Type string
}

func (a Float_expr) GenNodeName() string {
	return "float_expr"
}
func parse_float_expr(line string) (n Node) {
	return Float_expr{
		Op0:  findVal("op 0:", &line),
		Type: findVal("type:", &line),
	}
}
