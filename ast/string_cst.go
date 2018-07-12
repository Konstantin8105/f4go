package ast

type String_cst struct {
	Lngt string
	Strg string
	Type string
}

func (a String_cst) GenNodeName() string {
	return "string_cst"
}
func parse_string_cst(line string) (n Node) {
	return String_cst{
		Lngt: findVal("lngt:", &line),
		Strg: findVal("strg:", &line),
		Type: findVal("type:", &line),
	}
}
