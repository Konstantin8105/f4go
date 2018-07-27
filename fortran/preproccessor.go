package fortran

func preprocessor(b *[]byte) {

	// Example of possible error:
	// IF ( 2.LE.1) ...
	//       |
	//       +- error here, because it is not value "2."
	//          it is value "2"

	// ops := []string{
	// 	".LT.",
	// 	".GT.",
	// 	".LE.",
	// 	".GE.",
	// 	".NOT.",
	// 	".NE.",
	// 	".EQ.",
	// 	".AND.",
	// 	".OR.",
	// 	".TRUE.",
	// 	".FALSE.",
	// }
}
