package ast

type Modify_expr struct {
	Op0  string
	Op1  string
	Type string
}

func (a Modify_expr) GenNodeName() string {
	return "modify_expr"
}
func parse_modify_expr(line string) (n Node) {
	return Modify_expr{
		Op0:  findVal("op 0:", &line),
		Op1:  findVal("op 1:", &line),
		Type: findVal("type:", &line),
	}
}
