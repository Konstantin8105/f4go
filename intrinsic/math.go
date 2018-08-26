package intrinsic

func MIN(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func MAX(a, b int) int {
	if a > b {
		return a
	}
	return b
}
