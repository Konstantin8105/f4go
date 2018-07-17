
		package ast

		
type Gt_expr struct {
	Op0 string
	Op1 string
	Type string
}


		func (a Gt_expr) GenNodeName() string {
			return "gt_expr"
		}
		func parse_gt_expr(line string) (n Node) {
	return Gt_expr{
		Op0: findVal("op 0:", &line),
		Op1: findVal("op 1:", &line),
		Type: findVal("type:", &line),
	}
}
