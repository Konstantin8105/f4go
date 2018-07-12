package ast

type Record_type struct {
	Algn string
	Flds string
	Name string
	Size string
	Tag  string
}

func (a Record_type) GenNodeName() string {
	return "record_type"
}
func parse_record_type(line string) (n Node) {
	return Record_type{
		Algn: findVal("algn:", &line),
		Flds: findVal("flds:", &line),
		Name: findVal("name:", &line),
		Size: findVal("size:", &line),
		Tag:  findVal("tag :", &line),
	}
}
