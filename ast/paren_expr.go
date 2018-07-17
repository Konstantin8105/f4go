
		package ast

		
type Paren_expr struct {
	Op0 string
	Type string
}


		func (a Paren_expr) GenNodeName() string {
			return "paren_expr"
		}
		func parse_paren_expr(line string) (n Node) {
	return Paren_expr{
		Op0: findVal("op 0:", &line),
		Type: findVal("type:", &line),
	}
}
