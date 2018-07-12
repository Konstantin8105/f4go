package ast

type Reference_type struct {
	Algn string
	Qual string
	Refd string
	Size string
	Unql string
}

func (a Reference_type) GenNodeName() string {
	return "reference_type"
}
func parse_reference_type(line string) (n Node) {
	return Reference_type{
		Algn: findVal("algn:", &line),
		Qual: findVal("qual:", &line),
		Refd: findVal("refd:", &line),
		Size: findVal("size:", &line),
		Unql: findVal("unql:", &line),
	}
}
