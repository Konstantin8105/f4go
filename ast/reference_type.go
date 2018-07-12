package ast

type Reference_type struct {
	Qual string
	Unql string
	Size string
	Algn string
	Refd string
}

func (a Reference_type) GenNodeName() string {
	return "Reference_type "
}

func parse_reference_type(line string) (n Node) {
	return Reference_type{
		Qual: findVal("qual:", &line),
		Unql: findVal("unql:", &line),
		Size: findVal("size:", &line),
		Algn: findVal("algn:", &line),
		Refd: findVal("refd:", &line),
	}
}
