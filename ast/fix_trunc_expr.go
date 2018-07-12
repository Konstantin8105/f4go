package ast

type Fix_trunc_expr struct {
	Op0  string
	Type string
}

func (a Fix_trunc_expr) GenNodeName() string {
	return "fix_trunc_expr"
}
func parse_fix_trunc_expr(line string) (n Node) {
	return Fix_trunc_expr{
		Op0:  findVal("op 0:", &line),
		Type: findVal("type:", &line),
	}
}
