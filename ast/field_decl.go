package ast

type Field_decl struct {
	Algn string
	Bpos string
	Name string
	Scpe string
	Size string
	Type string
}

func (a Field_decl) GenNodeName() string {
	return "field_decl"
}
func parse_field_decl(line string) (n Node) {
	return Field_decl{
		Algn: findVal("algn:", &line),
		Bpos: findVal("bpos:", &line),
		Name: findVal("name:", &line),
		Scpe: findVal("scpe:", &line),
		Size: findVal("size:", &line),
		Type: findVal("type:", &line),
	}
}
