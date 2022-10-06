package main

type MEMORY struct {
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
	POINTER struct {
		NP *int64
		UP *int64
	}
}

var COMMON MEMORY

// !$Id:$
func PFACEV(IX *int, X *int, NDM *int, ILN *int, CT *int, IP *int, NFACE *int) {
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
	VISBL := new(bool)
	J := new(int)
	K := new(int)
	ILND := new(int)
	KIND := func() *[]float64 {
		arr := make([]float64, -1)
		return &arr
	}()
	//!      * * F E A P * * A Finite Element Analysis Program

	//!....  Copyright (c) 1984-2021: Regents of the University of California

	//!                               All rights reserved

	//!-----[--.----+----.----+----.-----------------------------------------]

	//!      Purpose: Plot visible exterior faces of 3-d objects

	//!      Inputs:

	//!         ix(4)      - List of face nodes

	//!         x(ndm,*)   - Nodal coordinates

	//!         iln(2)     - Line type data

	//!         ct         - Also plot back faces when > 0

	//!         ip(*)      - Sort data for hidden surface representations

	//!         nface      - Number of faces

	//!      Outputs:

	//!         none       - Plot outputs to screen/file

	//!-----[--.----+----.----+----.-----------------------------------------]

	HR = COMMON.COMBLK.HR
	MR = COMMON.COMBLK.MR
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
	//F4GO: NOT IMPLEMENTED :real ( KIND = 8 ) :: X ( NDM , * ) , XL ( 3 , 4 ) , CT
	//F4GO: NOT IMPLEMENTED :data ILND ( 1 ) / 1 /
	ILND(func() *int { y := 2; return &y }()) = (*(ILN)(func() *int { y := 2; return &y }()))
	//!     Plot face

	for (*J) = 1; (*J) <= 4; (*J)++ {
		for (*K) = 1; (*K) <= 3; (*K)++ {
			XL(K, J) = (*X(K, (IX)(J)))
		}
	}
	if (*VISBL(XL)) || ((IX)(func() *int { y := 1; return &y }()) == (IX)(func() *int { y := 4; return &y }()) && (IX)(func() *int { y := 2; return &y }()) == (IX)(func() *int { y := 3; return &y }())) {
		(*(NFACE)) = (*(NFACE)) + 1
		if (*CT) >= 0.0e0 {
			PLLINE((ILN))
			PLOTL(XL(func() *int { y := 1; return &y }(), func() *int { y := 4; return &y }()), XL(func() *int { y := 2; return &y }(), func() *int { y := 4; return &y }()), XL(func() *int { y := 3; return &y }(), func() *int { y := 4; return &y }()), func() *int { y := 3; return &y }())
			for (*J) = 1; (*J) <= 4; (*J)++ {
				PLOTL(XL(func() *int { y := 1; return &y }(), J), XL(func() *int { y := 2; return &y }(), J), XL(func() *int { y := 3; return &y }(), J), func() *int { y := 2; return &y }())
				MR(NP(func() *int { y := 66; return &y }()) + (IX)(J) - 1) = 1
			}
		}
	} else if (*CT) > 0.0e0 {
		(*(IP)) = 0
		PLLINE(ILND)
		PLOTL(XL(func() *int { y := 1; return &y }(), func() *int { y := 4; return &y }()), XL(func() *int { y := 2; return &y }(), func() *int { y := 4; return &y }()), XL(func() *int { y := 3; return &y }(), func() *int { y := 4; return &y }()), func() *int { y := 3; return &y }())
		for (*J) = 1; (*J) <= 4; (*J)++ {
			PLOTL(XL(func() *int { y := 1; return &y }(), J), XL(func() *int { y := 2; return &y }(), J), XL(func() *int { y := 3; return &y }(), J), func() *int { y := 2; return &y }())
		}
	} else {
		(*(IP)) = 0
	}
}
