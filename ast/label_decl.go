package ast

type Label_decl struct {
	Name string
	Note string
	Scpe string
	Srcp string
	Type string
}

func (a Label_decl) GenNodeName() string {
	return "label_decl"
}
func parse_label_decl(line string) (n Node) {
	return Label_decl{
		Name: findVal("name:", &line),
		Note: findVal("note:", &line),
		Scpe: findVal("scpe:", &line),
		Srcp: findVal("srcp:", &line),
		Type: findVal("type:", &line),
	}
}
