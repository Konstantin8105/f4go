package intrinsic

import (
	"math"
	"math/cmplx"
)

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

func DBLE(a int) float64 {
	return float64(a)
}

func ABS(a float64) float64 {
	return math.Abs(a)
}

func SIGN(a float64) float64 {
	if a < 0.0 {
		return -1
	}
	return 1
}
