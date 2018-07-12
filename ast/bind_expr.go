package ast

type Bind_expr struct {
	Body string
	Type string
	Vars string
}

func (a Bind_expr) GenNodeName() string {
	return "bind_expr"
}
func parse_bind_expr(line string) (n Node) {
	return Bind_expr{
		Body: findVal("body:", &line),
		Type: findVal("type:", &line),
		Vars: findVal("vars:", &line),
	}
}
