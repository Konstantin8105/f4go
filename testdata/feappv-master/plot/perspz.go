package main

import "github.com/Konstantin8105/f4go/intrinsic"

type MEMORY struct {
	IOFILE struct {
		KEEPFL *bool
		IOR    *int
		IOW    *int
	}
	PPERS struct {
		E     *float64
		EOLD  *float64
		TG    *float64
		VOLD  *float64
		Q     *float64
		XLBDA *float64
		ENORM *float64
		KPERS *int
	}
	PDATA4 struct {
		XMIN *float64
		XMAX *float64
		NZM1 *int
		NZM2 *int
		NFAC *int
		NFMX *int
		PDF  *int
	}
	POINTER struct {
		NP *int64
		UP *int64
	}
	NPOINTS struct {
		NPUD *int64
		NPID *int64
		NPIX *int64
		NPUU *int64
		NPXX *int64
		NPER *int64
		NPNP *int64
		NPEV *int64
		NPRN *int64
		NPTY *int64
	}
	PPOINTS struct {
		PLIX *int64
	}
	COMBLK struct {
		HR *float64
		MR *int
	}
}

var COMMON MEMORY

// !$Id:$
func PERSPZ(X *int, IX *int, IP *int, NEN1 *int, NEN *int, NDM *int, NUMNP *int, NUMEL *int) {
	IOR := new(int)
	IOW := new(int)
	COMMON.IOFILE.IOW = new(float64)
	COMMON.IOFILE.IOR = new(int)
	KEEPFL := new(bool)
	COMMON.IOFILE.KEEPFL = new(int)
	E := new(float64)
	EOLD := new(float64)
	TG := new(float64)
	VOLD := new(float64)
	Q := new(float64)
	XLBDA := new(float64)
	ENORM := new(float64)
	KPERS := new(int)
	COMMON.PPERS.KPERS = new(float64)
	COMMON.PPERS.ENORM = new(float64)
	COMMON.PPERS.XLBDA = func() *[][]float64 {
		arr := make([][]float64, 3)
		for u := 0; u < 3; u++ {
			arr[u] = make([]float64, 3)
		}
		return &arr
	}()
	COMMON.PPERS.Q = func() *[][]float64 {
		arr := make([][]float64, 3)
		for u := 0; u < 3; u++ {
			arr[u] = make([]float64, 3)
		}
		return &arr
	}()
	COMMON.PPERS.VOLD = func() *[]float64 {
		arr := make([]float64, 3)
		return &arr
	}()
	COMMON.PPERS.TG = func() *[]float64 {
		arr := make([]float64, 3)
		return &arr
	}()
	COMMON.PPERS.EOLD = func() *[]float64 {
		arr := make([]float64, 3)
		return &arr
	}()
	COMMON.PPERS.E = func() *[]int {
		arr := make([]int, 3)
		return &arr
	}()
	XMIN := new(float64)
	XMAX := new(float64)
	NZM1 := new(int)
	NZM2 := new(int)
	NFAC := new(int)
	NFMX := new(int)
	PDF := new(int)
	COMMON.PDATA4.PDF = func() *[]float64 {
		arr := make([]float64, 3)
		return &arr
	}()
	COMMON.PDATA4.NFMX = new(float64)
	COMMON.PDATA4.NFAC = new(float64)
	COMMON.PDATA4.NZM2 = new(float64)
	COMMON.PDATA4.NZM1 = new(float64)
	COMMON.PDATA4.XMAX = func() *[]float64 {
		arr := make([]float64, 3)
		return &arr
	}()
	COMMON.PDATA4.XMIN = func() *[]int {
		arr := make([]int, 3)
		return &arr
	}()
	NUM_NPS := new(int)
	NUM_UPS := new(int)
	NP := new(int64)
	UP := new(int64)
	COMMON.POINTER.UP = func() *[]float64 {
		arr := make([]float64, 200)
		return &arr
	}()
	COMMON.POINTER.NP = func() *[]int {
		arr := make([]int, 400)
		return &arr
	}()
	NPID := new(int64)
	NPIX := new(int64)
	NPUU := new(int64)
	NPXX := new(int64)
	NPER := new(int64)
	NPNP := new(int64)
	NPEV := new(int64)
	NPRN := new(int64)
	NPTY := new(int64)
	COMMON.NPOINTS.NPTY = new(float64)
	COMMON.NPOINTS.NPRN = new(float64)
	COMMON.NPOINTS.NPEV = new(float64)
	COMMON.NPOINTS.NPNP = new(float64)
	COMMON.NPOINTS.NPER = new(float64)
	COMMON.NPOINTS.NPXX = new(float64)
	COMMON.NPOINTS.NPUU = new(float64)
	COMMON.NPOINTS.NPIX = new(float64)
	COMMON.NPOINTS.NPID = new(int)
	NPUD := new(int64)
	COMMON.NPOINTS.NPUD = new(int)
	PLIX := new(int64)
	COMMON.PPOINTS.PLIX = new(int)
	HR := new(float64)
	MR := new(int)
	COMMON.COMBLK.MR = func() *[]float64 {
		arr := make([]float64, 1024)
		return &arr
	}()
	COMMON.COMBLK.HR = func() *[]int {
		arr := make([]int, 1024)
		return &arr
	}()
	I := new(int)
	II := new(int)
	N := new(int)
	NNE := new(int)
	IC := new(int)
	KIND := func() *[]float64 {
		arr := make([]float64, -1)
		return &arr
	}()
	KIND := func() *[]float64 {
		arr := make([]float64, -1)
		return &arr
	}()
	//!      * * F E A P * * A Finite Element Analysis Program

	//!....  Copyright (c) 1984-2021: Regents of the University of California

	//!                               All rights reserved

	//!-----[--.----+----.----+----.-----------------------------------------]

	//!      Purpose: Sort perspective projection of coordinates by z-values

	//!      Inputs:

	//!         x (ndm,*) - Nodal coordinates

	//!         ix(nen1,*)- Element connection list

	//!         ndm       - 1st dimension of x

	//!         nen1      - 1st dimension of ix

	//!         nen       - Number nodes connected to elements

	//!         numel     - Total number of elements

	//!         numnp     - Total number of nodal points

	//!      Outputs:

	//!         ip(*)     - Element order for z-coordinates

	//!-----[--.----+----.----+----.-----------------------------------------]

	IOR = COMMON.IOFILE.IOR
	IOW = COMMON.IOFILE.IOW
	KEEPFL = COMMON.IOFILE.KEEPFL
	E = COMMON.PPERS.E
	EOLD = COMMON.PPERS.EOLD
	TG = COMMON.PPERS.TG
	VOLD = COMMON.PPERS.VOLD
	Q = COMMON.PPERS.Q
	XLBDA = COMMON.PPERS.XLBDA
	ENORM = COMMON.PPERS.ENORM
	KPERS = COMMON.PPERS.KPERS
	XMIN = COMMON.PDATA4.XMIN
	XMAX = COMMON.PDATA4.XMAX
	NZM1 = COMMON.PDATA4.NZM1
	NZM2 = COMMON.PDATA4.NZM2
	NFAC = COMMON.PDATA4.NFAC
	NFMX = COMMON.PDATA4.NFMX
	PDF = COMMON.PDATA4.PDF
	(*NUM_NPS) = 400
	(*NUM_UPS) = 200
	//! int8

	NP = COMMON.POINTER.NP
	UP = COMMON.POINTER.UP
	NPID = COMMON.NPOINTS.NPID
	NPIX = COMMON.NPOINTS.NPIX
	NPUU = COMMON.NPOINTS.NPUU
	NPXX = COMMON.NPOINTS.NPXX
	NPER = COMMON.NPOINTS.NPER
	NPNP = COMMON.NPOINTS.NPNP
	NPEV = COMMON.NPOINTS.NPEV
	NPRN = COMMON.NPOINTS.NPRN
	NPTY = COMMON.NPOINTS.NPTY
	NPUD = COMMON.NPOINTS.NPUD
	PLIX = COMMON.PPOINTS.PLIX
	HR = COMMON.COMBLK.HR
	MR = COMMON.COMBLK.MR
	//F4GO: NOT IMPLEMENTED :real ( KIND = 8 ) :: T ( 3 ) , X ( NDM , NUMNP )
	//F4GO: NOT IMPLEMENTED :real ( KIND = 4 ) :: ZMAX , ZN ( NUMNP ) , ZE ( NUMEL )
	//!     Loop over data points and find projection

	for (*N) = 1; (*N) <= (*(NUMNP)); (*N)++ {
		if (*MR((*NPTY) - 1 + (*N))) >= 0 {
			for (*I) = 1; (*I) <= 3; (*I)++ {
				T(I) = X(I, N) - E(I)
			}
			ZN(N) = -real(XLBDA(func() *int { y := 3; return &y }(), func() *int { y := 1; return &y }())*T(func() *int { y := 1; return &y }()) + XLBDA(func() *int { y := 3; return &y }(), func() *int { y := 2; return &y }())*T(func() *int { y := 2; return &y }()) + XLBDA(func() *int { y := 3; return &y }(), func() *int { y := 3; return &y }())*T(func() *int { y := 3; return &y }()))
		} else {
			ZN(N) = 0.0e0
		}
	}
	//!     Search visible faces for depth sort

	for (*N) = 1; (*N) <= (*NFAC); (*N)++ {
		(*NNE) = (*(IP)(N))
		if (*NNE) > 0 && (*NNE) <= (*(NUMEL)) {
			if (*(IX)((NEN1), NNE)) > 0 {
				(*ZMAX) = 0.0e0
				for (*I) = 1; (*I) <= (*(NEN)); (*I)++ {
					(*II) = (intrinsic.ABS((IX)(I, NNE)))
					if (*II) > 0 && (*II) <= (*(NUMNP)) {
						(*ZMAX) = (intrinsic.MAX((*ZMAX), ZN(II)))
					}
				}
				ZE(NNE) = (*ZMAX)
			}
		}
	}
	//! n

	//!     Sort element plot order array "ip" to produce hidden surface

	MERGES(-1, ZE, func() *int { y := 1; return &y }(), (IP), NFAC, IC)
}
