package intrinsic

import "math/cmplx"

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

func CONJG(c complex64) complex64 {
	return complex64(cmplx.Conj(complex128(c)))
}

func DCONJG(c complex128) complex128 {
	return cmplx.Conj(c)
}
