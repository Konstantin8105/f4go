package main

import "github.com/Konstantin8105/f4go/intrinsic"

type MEMORY struct {
	PDATA0 struct {
		VMIN *float64
		VMAX *float64
	}
	PDATAS struct {
		ISYMM *int
		IQUAD *int
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
func MAXCOR(X *int, NDM *int, NUMNP *int) {
	VMIN := new(float64)
	VMAX := new(float64)
	COMMON.PDATA0.VMAX = func() *[]float64 {
		arr := make([]float64, 3)
		return &arr
	}()
	COMMON.PDATA0.VMIN = func() *[]int {
		arr := make([]int, 3)
		return &arr
	}()
	ISYMM := new(int)
	IQUAD := new(int)
	COMMON.PDATAS.IQUAD = func() *[][][]float64 {
		arr := make([][][]float64, 2)
		for u := 0; u < 2; u++ {
			arr[u] = make([][]float64, 2)
			for w := 0; w < 2; w++ {
				arr[u][w] = make([]float64, 2)
			}
		}
		return &arr
	}()
	COMMON.PDATAS.ISYMM = func() *[][]int {
		arr := make([][]int, 3)
		for u := 0; u < 3; u++ {
			arr[u] = make([]int, 2)
		}
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
	KIND := func() *[]float64 {
		arr := make([]float64, -1)
		return &arr
	}()
	//!      * * F E A P * * A Finite Element Analysis Program

	//!....  Copyright (c) 1984-2021: Regents of the University of California

	//!                               All rights reserved

	//!-----[--.----+----.----+----.-----------------------------------------]

	//!      Purpose: Determine max/min coordinates for a perspective

	//!               view selection

	//!      Inputs:

	//!         x(ndm,*)  - Nodal coordinates (may be in deformed state)

	//!         ndm       - Spatial dimension

	//!         numnp     - Number of nodes in mesh

	//!      Outputs:

	//!         none      - Output through common /pdata0/

	//!-----[--.----+----.----+----.-----------------------------------------]

	VMIN = COMMON.PDATA0.VMIN
	VMAX = COMMON.PDATA0.VMAX
	ISYMM = COMMON.PDATAS.ISYMM
	IQUAD = COMMON.PDATAS.IQUAD
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
	//!     Determine max/min coordinates for a perspective view selection

	(*II) = (intrinsic.MIN((*(NDM)), int(3)))
	for (*I) = 1; (*I) <= (*II); (*I)++ {
		VMIN(I) = (*X(I, func() *int { y := 1; return &y }()))
		for (*N) = 1; (*N) <= (*(NUMNP)); (*N)++ {
			VMIN(I) = (intrinsic.MAX(VMIN(I), X(I, N)))
		}
		VMAX(I) = (*VMIN(I))
		for (*N) = 1; (*N) <= (*(NUMNP)); (*N)++ {
			if (*MR((*NPTY) - 1 + (*N))) >= 0 {
				VMIN(I) = (intrinsic.MIN(VMIN(I), X(I, N)))
			}
		}
	}
}
