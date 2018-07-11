package ast

type Translation_unit_decl struct {
}

func (a Translation_unit_decl) GenNodeName() string {
	return "Translation_unit_decl "
}

func parse_translation_unit_decl(line string) (n Node) {
	return Translation_unit_decl{}
}
