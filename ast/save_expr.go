
		package ast

		
type Save_expr struct {
	Op0 string
	Type string
}


		func (a Save_expr) GenNodeName() string {
			return "save_expr"
		}
		func parse_save_expr(line string) (n Node) {
	return Save_expr{
		Op0: findVal("op 0:", &line),
		Type: findVal("type:", &line),
	}
}
