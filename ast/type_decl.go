package ast

type Type_decl struct {
	Name string
	Srcp string
	Type string
}

func (a Type_decl) GenNodeName() string {
	return "type_decl"
}
func parse_type_decl(line string) (n Node) {
	return Type_decl{
		Name: findVal("name:", &line),
		Srcp: findVal("srcp:", &line),
		Type: findVal("type:", &line),
	}
}
