package ast

type Function_type struct {
	Algn string
	Prms string
	Retn string
	Size string
}

func (a Function_type) GenNodeName() string {
	return "function_type"
}
func parse_function_type(line string) (n Node) {
	return Function_type{
		Algn: findVal("algn:", &line),
		Prms: findVal("prms:", &line),
		Retn: findVal("retn:", &line),
		Size: findVal("size:", &line),
	}
}
