package ast

type Return_expr struct {
	Expr string
	Type string
}

func (a Return_expr) GenNodeName() string {
	return "return_expr"
}
func parse_return_expr(line string) (n Node) {
	return Return_expr{
		Expr: findVal("expr:", &line),
		Type: findVal("type:", &line),
	}
}
