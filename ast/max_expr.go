
		package ast

		
type Max_expr struct {
	Op0 string
	Op1 string
	Type string
}


		func (a Max_expr) GenNodeName() string {
			return "max_expr"
		}
		func parse_max_expr(line string) (n Node) {
	return Max_expr{
		Op0: findVal("op 0:", &line),
		Op1: findVal("op 1:", &line),
		Type: findVal("type:", &line),
	}
}
