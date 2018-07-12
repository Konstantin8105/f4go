package ast

type Function_decl struct {
	Args string
	Body string
	Link string
	Mngl string
	Name string
	Scpe string
	Srcp string
	Type string
}

func (a Function_decl) GenNodeName() string {
	return "function_decl"
}
func parse_function_decl(line string) (n Node) {
	return Function_decl{
		Args: findVal("args:", &line),
		Body: findVal("body:", &line),
		Link: findVal("link:", &line),
		Mngl: findVal("mngl:", &line),
		Name: findVal("name:", &line),
		Scpe: findVal("scpe:", &line),
		Srcp: findVal("srcp:", &line),
		Type: findVal("type:", &line),
	}
}
