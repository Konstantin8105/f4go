
		package ast

		
type Ne_expr struct {
	Op0 string
	Op1 string
	Type string
}


		func (a Ne_expr) GenNodeName() string {
			return "ne_expr"
		}
		func parse_ne_expr(line string) (n Node) {
	return Ne_expr{
		Op0: findVal("op 0:", &line),
		Op1: findVal("op 1:", &line),
		Type: findVal("type:", &line),
	}
}
