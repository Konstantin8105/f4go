package ast

type Goto_expr struct {
	Labl string
	Type string
}

func (a Goto_expr) GenNodeName() string {
	return "goto_expr"
}
func parse_goto_expr(line string) (n Node) {
	return Goto_expr{
		Labl: findVal("labl:", &line),
		Type: findVal("type:", &line),
	}
}
