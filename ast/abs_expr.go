
		package ast

		
type Abs_expr struct {
	Op0 string
	Type string
}


		func (a Abs_expr) GenNodeName() string {
			return "abs_expr"
		}
		func parse_abs_expr(line string) (n Node) {
	return Abs_expr{
		Op0: findVal("op 0:", &line),
		Type: findVal("type:", &line),
	}
}
