package ast

type Void_type struct {
	Algn string
	Name string
	Qual string
	Unql string
}

func (a Void_type) GenNodeName() string {
	return "void_type"
}
func parse_void_type(line string) (n Node) {
	return Void_type{
		Algn: findVal("algn:", &line),
		Name: findVal("name:", &line),
		Qual: findVal("qual:", &line),
		Unql: findVal("unql:", &line),
	}
}
