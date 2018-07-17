
		package ast

		
type Rdiv_expr struct {
	Op0 string
	Op1 string
	Type string
}


		func (a Rdiv_expr) GenNodeName() string {
			return "rdiv_expr"
		}
		func parse_rdiv_expr(line string) (n Node) {
	return Rdiv_expr{
		Op0: findVal("op 0:", &line),
		Op1: findVal("op 1:", &line),
		Type: findVal("type:", &line),
	}
}
