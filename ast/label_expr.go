package ast

type Label_expr struct {
	Name string
	Type string
}

func (a Label_expr) GenNodeName() string {
	return "label_expr"
}
func parse_label_expr(line string) (n Node) {
	return Label_expr{
		Name: findVal("name:", &line),
		Type: findVal("type:", &line),
	}
}
