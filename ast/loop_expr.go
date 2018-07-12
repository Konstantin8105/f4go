package ast

type Loop_expr struct {
	Body string
	Type string
}

func (a Loop_expr) GenNodeName() string {
	return "loop_expr"
}
func parse_loop_expr(line string) (n Node) {
	return Loop_expr{
		Body: findVal("body:", &line),
		Type: findVal("type:", &line),
	}
}
