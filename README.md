# f4go


[![Build Status](https://travis-ci.org/Konstantin8105/f4go.svg?branch=master)](https://travis-ci.org/Konstantin8105/f4go)
[![Go Report Card](https://goreportcard.com/badge/github.com/Konstantin8105/f4go)](https://goreportcard.com/report/github.com/Konstantin8105/f4go)
[![codecov](https://codecov.io/gh/Konstantin8105/f4go/branch/master/graph/badge.svg)](https://codecov.io/gh/Konstantin8105/f4go)
[![GitHub license](https://img.shields.io/badge/license-MIT-blue.svg)](https://raw.githubusercontent.com/Konstantin8105/f4go/master/LICENSE)
[![GoDoc](https://godoc.org/github.com/Konstantin8105/f4go/fortran?status.svg)](https://godoc.org/github.com/Konstantin8105/f4go/fortran)
[![Maintainability](https://api.codeclimate.com/v1/badges/b8d0bb5533207cce5ed3/maintainability)](https://codeclimate.com/github/Konstantin8105/f4go/maintainability)

# Example of use

```cmd
> # Install golang
> # Compile f4go
> go get -u github.com/Konstantin8105/f4go
> cd $GOPATH/src/github.com/Konstantin8105/f4go
> go build
> ./f4go ./testdata/blas/caxpy.f
> # Look on Go result source
> less ./testdata/blas/caxpy.go
```

# Transpiling fortran code to golang code

Present result:
```fortran
*> \brief \b CAXPY
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE CAXPY(N,CA,CX,INCX,CY,INCY)
*
*       .. Scalar Arguments ..
*       COMPLEX CA
*       INTEGER INCX,INCY,N
*       ..
*       .. Array Arguments ..
*       COMPLEX CX(*),CY(*)
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*>    CAXPY constant times a vector plus a vector.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>         number of elements in input vector(s)
*> \endverbatim
*>
*> \param[in] CA
*> \verbatim
*>          CA is COMPLEX
*>           On entry, CA specifies the scalar alpha.
*> \endverbatim
*>
*> \param[in] CX
*> \verbatim
*>          CX is COMPLEX array, dimension ( 1 + ( N - 1 )*abs( INCX ) )
*> \endverbatim
*>
*> \param[in] INCX
*> \verbatim
*>          INCX is INTEGER
*>         storage spacing between elements of CX
*> \endverbatim
*>
*> \param[in,out] CY
*> \verbatim
*>          CY is COMPLEX array, dimension ( 1 + ( N - 1 )*abs( INCY ) )
*> \endverbatim
*>
*> \param[in] INCY
*> \verbatim
*>          INCY is INTEGER
*>         storage spacing between elements of CY
*> \endverbatim
*
*  Authors:
*  ========
*
*> \author Univ. of Tennessee
*> \author Univ. of California Berkeley
*> \author Univ. of Colorado Denver
*> \author NAG Ltd.
*
*> \date November 2017
*
*> \ingroup complex_blas_level1
*
*> \par Further Details:
*  =====================
*>
*> \verbatim
*>
*>     jack dongarra, linpack, 3/11/78.
*>     modified 12/3/93, array(1) declarations changed to array(*)
*> \endverbatim
*>
*  =====================================================================
      SUBROUTINE CAXPY(N,CA,CX,INCX,CY,INCY)
*
*  -- Reference BLAS level1 routine (version 3.8.0) --
*  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     November 2017
*
*     .. Scalar Arguments ..
      COMPLEX CA
      INTEGER INCX,INCY,N
*     ..
*     .. Array Arguments ..
      COMPLEX CX(*),CY(*)
*     ..
*
*  =====================================================================
*
*     .. Local Scalars ..
      INTEGER I,IX,IY
*     ..
*     .. External Functions ..
      REAL SCABS1
      EXTERNAL SCABS1
*     ..
      IF (N.LE.0) RETURN
      IF (SCABS1(CA).EQ.0.0E+0) RETURN
      IF (INCX.EQ.1 .AND. INCY.EQ.1) THEN
*
*        code for both increments equal to 1
*
         DO I = 1,N
            CY(I) = CY(I) + CA*CX(I)
         END DO
      ELSE
*
*        code for unequal increments or equal increments
*          not equal to 1
*
         IX = 1
         IY = 1
         IF (INCX.LT.0) IX = (-N+1)*INCX + 1
         IF (INCY.LT.0) IY = (-N+1)*INCY + 1
         DO I = 1,N
            CY(IY) = CY(IY) + CA*CX(IX)
            IX = IX + INCX
            IY = IY + INCY
         END DO
      END IF
*
      RETURN
      END
```

Go code:

```golang
package main

//*> \brief \b CAXPY
//*
//*  =========== DOCUMENTATION ===========
//*
//* Online html documentation available at
//*            http://www.netlib.org/lapack/explore-html/
//*
//*  Definition:
//*  ===========
//*
//*       SUBROUTINE CAXPY(N,CA,CX,INCX,CY,INCY)
//*
//*       .. Scalar Arguments ..
//*       COMPLEX CA
//*       INTEGER INCX,INCY,N
//*       ..
//*       .. Array Arguments ..
//*       COMPLEX CX(*),CY(*)
//*       ..
//*
//*
//*> \par Purpose:
//*  =============
//*>
//*> \verbatim
//*>
//*>    CAXPY constant times a vector plus a vector.
//*> \endverbatim
//*
//*  Arguments:
//*  ==========
//*
//*> \param[in] N
//*> \verbatim
//*>          N is INTEGER
//*>         number of elements in input vector(s)
//*> \endverbatim
//*>
//*> \param[in] CA
//*> \verbatim
//*>          CA is COMPLEX
//*>           On entry, CA specifies the scalar alpha.
//*> \endverbatim
//*>
//*> \param[in] CX
//*> \verbatim
//*>          CX is COMPLEX array, dimension ( 1 + ( N - 1 )*abs( INCX ) )
//*> \endverbatim
//*>
//*> \param[in] INCX
//*> \verbatim
//*>          INCX is INTEGER
//*>         storage spacing between elements of CX
//*> \endverbatim
//*>
//*> \param[in,out] CY
//*> \verbatim
//*>          CY is COMPLEX array, dimension ( 1 + ( N - 1 )*abs( INCY ) )
//*> \endverbatim
//*>
//*> \param[in] INCY
//*> \verbatim
//*>          INCY is INTEGER
//*>         storage spacing between elements of CY
//*> \endverbatim
//*
//*  Authors:
//*  ========
//*
//*> \author Univ. of Tennessee
//*> \author Univ. of California Berkeley
//*> \author Univ. of Colorado Denver
//*> \author NAG Ltd.
//*
//*> \date November 2017
//*
//*> \ingroup complex_blas_level1
//*
//*> \par Further Details:
//*  =====================
//*>
//*> \verbatim
//*>
//*>     jack dongarra, linpack, 3/11/78.
//*>     modified 12/3/93, array(1) declarations changed to array(*)
//*> \endverbatim
//*>
//*  =====================================================================
func CAXPY(N *int, CA *complex128, CX *[]complex128, INCX *int, CY *[]complex128, INCY *int) {
	I := new(int)
	IX := new(int)
	IY := new(int)
	//*
	//*  -- Reference BLAS level1 routine (version 3.8.0) --
	//*  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
	//*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
	//*     November 2017
	//*
	//*     .. Scalar Arguments ..
	//*     ..
	//*     .. Array Arguments ..
	//*     ..
	//*
	//*  =====================================================================
	//*
	//*     .. Local Scalars ..
	//*     ..
	//*     .. External Functions ..
	//*     ..
	if (*(N)) <= 0 {
		return
	}
	if (*SCABS1((CA))) == 0.0e+0 {
		return
	}
	if (*(INCX)) == 1 && (*(INCY)) == 1 {
		//*
		//*        code for both increments equal to 1
		//*
		for (*I) = 1; (*I) <= (*(N)); (*I)++ {
			(*(CY))[(*I)-(1)] = (*(CY))[(*I)-(1)] + (*(CA))*(*(CX))[(*I)-(1)]
		}
	} else {
		//*
		//*        code for unequal increments or equal increments
		//*          not equal to 1
		//*
		(*IX) = 1
		(*IY) = 1
		if (*(INCX)) < 0 {
			(*IX) = (-(*(N))+1)*(*(INCX)) + 1
		}
		if (*(INCY)) < 0 {
			(*IY) = (-(*(N))+1)*(*(INCY)) + 1
		}
		for (*I) = 1; (*I) <= (*(N)); (*I)++ {
			(*(CY))[(*IY)-(1)] = (*(CY))[(*IY)-(1)] + (*(CA))*(*(CX))[(*IX)-(1)]
			(*IX) = (*IX) + (*(INCX))
			(*IY) = (*IY) + (*(INCY))
		}
	}
	//*
	return
}
```

### Notes

Fortran 77 | Golang
---------- | ------------
all arguments of function are pointers | ?
all arguments of intrisic function are pointers | ?
all internal function variables are pointers | ?


**IDENT**:
* constants
* arrays, matrixes

Operations:
* assign
* initialization
* boolean

### Fortran test sources

* [**LAPACK from lapack3.8.0**](http://netlib.org/lapack/index.html)
