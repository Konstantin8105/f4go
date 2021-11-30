package intrinsic

import (
	"fmt"
	"math"
	"math/cmplx"
)

const (
	eps = 1.1920928955078125e-07 // math.Pow(2,-23) = 1.1920928955078125e-07 (EPSILON)
)

func MIN(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func castToFloat64(w interface{}) float64 {
	switch v := w.(type) {
	case float64:
		return v
	case *float64:
		return *v
	case float32:
		return float64(v)
	case *float32:
		return float64(*v)
	case int:
		return float64(v)
	case *int:
		return float64(*v)
		// 	case complex128:
		// 		r := real(v)
		// 		i := imag(v)
		// 		return float64(math.Mod(r, 2) - math.Mod(i, 2))
		// 	case *complex128:
		// 		r := real(*v)
		// 		i := imag(*v)
		// 		return float64(math.Mod(r, 2) - math.Mod(i, 2))
	default:
		panic(fmt.Errorf("cannot cast: %#v", w))
	}
}

func SQRT(a interface{}) float64 {
	A := castToFloat64(a)
	return math.Sqrt(A)
}

func MAX(a, b interface{}) float64 {
	return math.Max(castToFloat64(a), castToFloat64(b))
}

func EPSILON(f float64) float64 {
	return eps
}

func CONJG(c complex128) complex128 {
	return cmplx.Conj(complex128(c))
}

func DCONJG(c complex128) complex128 {
	return cmplx.Conj(c)
}

func DBLE(a interface{}) float64 {
	switch a := a.(type) {
	case int:
		return float64(a)
	case int32:
		return float64(a)
	case int64:
		return float64(a)
	case float32:
		return float64(a)
	case complex64:
		return float64(real(a))
	case complex128:
		return float64(real(a))
	case float64:
		return a
	}
	panic(fmt.Errorf("Cannot find type : %T", a))
}

func ABS(a interface{}) float64 {
	return math.Abs(castToFloat64(a))
}

func CABS(a complex128) float64 {
	return cmplx.Abs(a)
}

func SIGN(a float64) float64 {
	return math.Copysign(1, a)
}

func MOD(a, b int) int {
	return a % b
}

func CMPLX(a interface{}) complex128 {
	A := castToFloat64(a)
	return complex(A, 0)
}
