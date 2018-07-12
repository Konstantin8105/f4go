package ast

type Var_decl struct {
	Algn string
	Init string
	Name string
	Note string
	Scpe string
	Size string
	Srcp string
	Type string
	Used string
}

func (a Var_decl) GenNodeName() string {
	return "var_decl"
}
func parse_var_decl(line string) (n Node) {
	return Var_decl{
		Algn: findVal("algn:", &line),
		Init: findVal("init:", &line),
		Name: findVal("name:", &line),
		Note: findVal("note:", &line),
		Scpe: findVal("scpe:", &line),
		Size: findVal("size:", &line),
		Srcp: findVal("srcp:", &line),
		Type: findVal("type:", &line),
		Used: findVal("used:", &line),
	}
}
