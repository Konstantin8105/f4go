package ast

type Parm_decl struct {
	Algn string
	Argt string
	Name string
	Scpe string
	Size string
	Srcp string
	Type string
	Used string
}

func (a Parm_decl) GenNodeName() string {
	return "parm_decl"
}
func parse_parm_decl(line string) (n Node) {
	return Parm_decl{
		Algn: findVal("algn:", &line),
		Argt: findVal("argt:", &line),
		Name: findVal("name:", &line),
		Scpe: findVal("scpe:", &line),
		Size: findVal("size:", &line),
		Srcp: findVal("srcp:", &line),
		Type: findVal("type:", &line),
		Used: findVal("used:", &line),
	}
}
