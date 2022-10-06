package main

type MEMORY struct {
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
}

var COMMON MEMORY

// !$Id:$
func PLINE(X *int, IE *int, IX *int, ID *int, IP *int, NUMNP *int, NUMEL *int, NDM *int, NEN1 *int, NEN *int, NIE *int, CT *int, ISW *bool) {
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
	SETVAR := new(bool)
	PALLOC := new(bool)
	KIND := func() *[]float64 {
		arr := make([]float64, -1)
		return &arr
	}()
	//!      * * F E A P * * A Finite Element Analysis Program

	//!....  Copyright (c) 1984-2021: Regents of the University of California

	//!                               All rights reserved

	//!-----[--.----+----.----+----.-----------------------------------------]

	//!      Purpose: Mesh plot and outline driver

	//!      Inputs:

	//!         x(ndm,*)  - Nodal coordinates

	//!         ie(nie,*) - Assembly data for material sets

	//!         ix(nen1,*)- Element nodal connection list

	//!         id(*)     - Number of elements attached to nodes

	//!         ip(*)     - Sorted element order for each quadrant

	//!         numnp     - Number of nodes in mesh

	//!         numel     - Number of elements in mesh

	//!         ndm       - Dimesion of x array

	//!         nen1      - Dimesion of ix array

	//!         nie       - Dimesion of ie array

	//!         ct(*)     - Color changing

	//!         isw       - Flag, Plot mesh if true, otherwise outline

	//!      Outputs:

	//!         none      - Plot output to screen/file

	//!-----[--.----+----.----+----.-----------------------------------------]

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
	//F4GO: NOT IMPLEMENTED :real ( KIND = 8 ) :: X ( * ) , CT ( * )
	//!     Determine necessary storage for mesh lines and allocate storage

	XCOMPP((IE), (IX), (ID), (NIE), (NEN1), (NEN), (NUMNP), (NUMEL))
	(*SETVAR) = (*PALLOC(func() *int { y := 112; return &y }(), func() *[]byte { y := []byte("TEMP2"); return &y }(), (ID)((*(NUMNP))+1), func() *int { y := 1; return &y }()))
	//!     Draw mesh

	XPLINE(X, (IE), (IX), (ID), MR(NP(func() *int { y := 112; return &y }())), (IP), (NUMNP), (NUMEL), (NDM), (NEN1), (NEN), (NIE), CT(func() *int { y := 1; return &y }()), (ISW))
	//!     Delete storage for mesh lines

	(*SETVAR) = (*PALLOC(func() *int { y := 112; return &y }(), func() *[]byte { y := []byte("TEMP2"); return &y }(), func() *int { y := 0; return &y }(), func() *int { y := 1; return &y }()))
}
