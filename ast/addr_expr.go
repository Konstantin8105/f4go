package ast

type Addr_expr struct {
	Op0  string
	Type string
}

func (a Addr_expr) GenNodeName() string {
	return "addr_expr"
}
func parse_addr_expr(line string) (n Node) {
	return Addr_expr{
		Op0:  findVal("op 0:", &line),
		Type: findVal("type:", &line),
	}
}
