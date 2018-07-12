package ast

type Const_decl struct {
	Cnst string
	Scpe string
	Srcp string
	Type string
}

func (a Const_decl) GenNodeName() string {
	return "const_decl"
}
func parse_const_decl(line string) (n Node) {
	return Const_decl{
		Cnst: findVal("cnst:", &line),
		Scpe: findVal("scpe:", &line),
		Srcp: findVal("srcp:", &line),
		Type: findVal("type:", &line),
	}
}
