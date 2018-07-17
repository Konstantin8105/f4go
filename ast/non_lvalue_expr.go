
		package ast

		
type Non_lvalue_expr struct {
	Op0 string
	Type string
}


		func (a Non_lvalue_expr) GenNodeName() string {
			return "non_lvalue_expr"
		}
		func parse_non_lvalue_expr(line string) (n Node) {
	return Non_lvalue_expr{
		Op0: findVal("op 0:", &line),
		Type: findVal("type:", &line),
	}
}
