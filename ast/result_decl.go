package ast

type Result_decl struct {
	Algn string
	Name string
	Note string
	Scpe string
	Size string
	Srcp string
	Type string
}

func (a Result_decl) GenNodeName() string {
	return "result_decl"
}
func parse_result_decl(line string) (n Node) {
	return Result_decl{
		Algn: findVal("algn:", &line),
		Name: findVal("name:", &line),
		Note: findVal("note:", &line),
		Scpe: findVal("scpe:", &line),
		Size: findVal("size:", &line),
		Srcp: findVal("srcp:", &line),
		Type: findVal("type:", &line),
	}
}
