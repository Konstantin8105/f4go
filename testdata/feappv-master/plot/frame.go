package main

import "github.com/Konstantin8105/f4go/intrinsic"

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
	PDATA1 struct {
		SCALEF *float64
		SCALEG *float64
		S0     *float64
		DX     *float64
		SX     *float64
		FACT   *float64
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
	PDATXT struct {
		DTEXT *float64
		XSYC  *float64
		JX1   *int
		JY1   *int
	}
	POINTER struct {
		NP *int64
		UP *int64
	}
}

var COMMON MEMORY

// !$Id:$
func FRAME(X *int, NDM *int, NUMNP *int, ISSW *int) {
	SCALEF := new(float64)
	SCALEG := new(float64)
	S0 := new(float64)
	DX := new(float64)
	SX := new(float64)
	FACT := new(float64)
	COMMON.PDATA1.FACT = new(float64)
	COMMON.PDATA1.SX = func() *[]float64 {
		arr := make([]float64, 2)
		return &arr
	}()
	COMMON.PDATA1.DX(2) = new(float64)
	COMMON.PDATA1.S0 = func() *[]float64 {
		arr := make([]float64, 2)
		return &arr
	}()
	COMMON.PDATA1.SCALEG = new(float64)
	COMMON.PDATA1.SCALEF = new(int)
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
	DTEXT := new(float64)
	XSYC := new(float64)
	JX1 := new(int)
	JY1 := new(int)
	COMMON.PDATXT.JY1 = new(float64)
	COMMON.PDATXT.JX1 = new(float64)
	COMMON.PDATXT.XSYC = func() *[]float64 {
		arr := make([]float64, 3)
		return &arr
	}()
	COMMON.PDATXT.DTEXT = new(int)
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
	ERRV := new(bool)
	ISW := new(int)
	II := new(int)
	IJ := new(int)
	I := new(int)
	J := new(int)
	N := new(int)
	KIND := func() *[]float64 {
		arr := make([]float64, -1)
		return &arr
	}()
	IP := new(int)
	KIND := func() *[]float64 {
		arr := make([]float64, -1)
		return &arr
	}()
	KIND := func() *[]float64 {
		arr := make([]float64, -1)
		return &arr
	}()
	RP := new(float64)
	//!      * * F E A P * * A Finite Element Analysis Program

	//!....  Copyright (c) 1984-2021: Regents of the University of California

	//!                               All rights reserved

	//!-----[--.----+----.----+----.-----------------------------------------]

	//!      Purpose: Computes scaling values for plot screen area

	//!      Inputs:

	//!         x(ndm,*)  - Nodal coordinates of mesh

	//!         ndm       - Spatial dimension of mesh

	//!         numnp     - Number of nodes in mesh

	//!         issw      - Switch: < 0 for no reflections;

	//!                             > 0 for reflections

	//!      Outputs:

	//!         none      - Data stored in common blocks

	//!-----[--.----+----.----+----.-----------------------------------------]

	SCALEF = COMMON.PDATA1.SCALEF
	SCALEG = COMMON.PDATA1.SCALEG
	S0 = COMMON.PDATA1.S0
	DX = COMMON.PDATA1.DX
	SX = COMMON.PDATA1.SX
	FACT = COMMON.PDATA1.FACT
	XMIN = COMMON.PDATA4.XMIN
	XMAX = COMMON.PDATA4.XMAX
	NZM1 = COMMON.PDATA4.NZM1
	NZM2 = COMMON.PDATA4.NZM2
	NFAC = COMMON.PDATA4.NFAC
	NFMX = COMMON.PDATA4.NFMX
	PDF = COMMON.PDATA4.PDF
	DTEXT = COMMON.PDATXT.DTEXT
	XSYC = COMMON.PDATXT.XSYC
	JX1 = COMMON.PDATXT.JX1
	JY1 = COMMON.PDATXT.JY1
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
	//F4GO: NOT IMPLEMENTED :real ( KIND = 8 ) :: XCEN , XW1 , XW2
	//F4GO: NOT IMPLEMENTED :real ( KIND = 8 ) :: X ( NDM , * ) , XMN ( 3 ) , XMX ( 3 ) , XMINO ( 3 ) , XMAXO ( 3 )
	//F4GO: NOT IMPLEMENTED :real ( KIND = 8 ) :: XP ( 3 , 8 ) , XPM ( 3 , 2 ) , RP ( 3 , 6 )
	//F4GO: NOT IMPLEMENTED :data RP / 1.e0 , 0.e0 , 0.e0 , 0.e0 , 1.e0 , 0.e0 , 1.e0 , 1.e0 , 0.e0 , 1.e0 , 0.e0 , 1.e0 , 0.e0 , 1.e0 , 1.e0 , 0.e0 , 0.e0 , 1.e0 /
	//!     Determine window coordinates

	(*ISW) = (intrinsic.ABS((*(ISSW))))
	DX(func() *int { y := 2; return &y }()) = 0.e0
	SX(func() *int { y := 2; return &y }()) = 0.e0
	(*II) = (intrinsic.MIN((*(NDM)), int(3)))
	(*IJ) = (intrinsic.MIN((*(NDM)), int(2)))
	if (*ISW) == 1 {
		PZERO(XMIN, func() *int { y := 3; return &y }())
		PZERO(XMAX, func() *int { y := 3; return &y }())
		for (*I) = 1; (*I) <= (*II); (*I)++ {
			XMIN(I) = (*X(I, func() *int { y := 1; return &y }()))
			for (*N) = 1; (*N) <= (*(NUMNP)); (*N)++ {
				XMIN(I) = (intrinsic.MAX(XMIN(I), X(I, N)))
			}
			XMAX(I) = (*X(I, func() *int { y := 1; return &y }()))
		}
	} else {
		for (*I) = 1; (*I) <= (*II); (*I)++ {
			XMIN(I) = (*XMINO(I))
			XMAX(I) = (*XMAXO(I))
		}
	}
	for (*N) = 1; (*N) <= (*(NUMNP)); (*N)++ {
		if (*MR((*NPTY) - 1 + (*N))) >= 0 {
			for (*I) = 1; (*I) <= (*II); (*I)++ {
				XMIN(I) = (intrinsic.MIN(XMIN(I), X(I, N)))
				XMAX(I) = (intrinsic.MAX(XMAX(I), X(I, N)))
				if (*ISW) == 1 {
					XMINO(I) = (*XMIN(I))
					XMAXO(I) = (*XMAX(I))
				}
			}
		}
	}
	//!     Perform reflections for window scaling

	if (*(ISSW)) > 0 {
		//!       Cartesian view

		for (*I) = 1; (*I) <= (*II); (*I)++ {
			XMIN(I) = (*MIN(XMIN(I), (XMIN(I)-XSYC(I))+XSYC(I), (XMAX(I)-XSYC(I))+XSYC(I)))
			XMAX(I) = (*MAX(XMAX(I), (XMIN(I)-XSYC(I))+XSYC(I), (XMAX(I)-XSYC(I))+XSYC(I)))
		}
		//!     Perspective view

	} else {
		for (*I) = 1; (*I) <= 3; (*I)++ {
			XP(I, func() *int { y := 1; return &y }()) = (XMIN(I) - XSYC(I)) + XSYC(I)
			XP(I, func() *int { y := 2; return &y }()) = (XMAX(I) - XSYC(I)) + XSYC(I)
		}
		for (*J) = 1; (*J) <= 6; (*J)++ {
			for (*I) = 1; (*I) <= 3; (*I)++ {
				XP(I, (*J)+2) = XP(I, func() *int { y := 1; return &y }()) + RP(I, J)*(XP(I, func() *int { y := 2; return &y }())-XP(I, func() *int { y := 1; return &y }()))
			}
		}
		for (*I) = 1; (*I) <= 8; (*I)++ {
			IP(I) = 1
		}
		PERSPJ(XP, IP, func() *int { y := 8; return &y }(), ERRV)
		for (*I) = 1; (*I) <= 3; (*I)++ {
			XPM(I, func() *int { y := 1; return &y }()) = (*MAX(XP(I, func() *int { y := 1; return &y }()), XP(I, func() *int { y := 2; return &y }()), XP(I, func() *int { y := 3; return &y }()), XP(I, func() *int { y := 4; return &y }()), XP(I, func() *int { y := 5; return &y }()), XP(I, func() *int { y := 6; return &y }()), XP(I, func() *int { y := 7; return &y }()), XP(I, func() *int { y := 8; return &y }())))
			XPM(I, func() *int { y := 2; return &y }()) = (*MIN(XP(I, func() *int { y := 1; return &y }()), XP(I, func() *int { y := 2; return &y }()), XP(I, func() *int { y := 3; return &y }()), XP(I, func() *int { y := 4; return &y }()), XP(I, func() *int { y := 5; return &y }()), XP(I, func() *int { y := 6; return &y }()), XP(I, func() *int { y := 7; return &y }()), XP(I, func() *int { y := 8; return &y }())))
		}
		for (*I) = 1; (*I) <= 3; (*I)++ {
			XMAX(I) = (*XPM(I, func() *int { y := 1; return &y }()))
			XMIN(I) = (*XPM(I, func() *int { y := 2; return &y }()))
		}
	}
	//!     Plot region determination

	for (*I) = 1; (*I) <= (*IJ); (*I)++ {
		(*XW1) = (*XMIN(I))
		(*XW2) = (*XMAX(I))
		//!       Modify window if nzm1 or nzm2 are nonzero

		if (*NZM1) > 0 && (*NZM1) <= (*(NUMNP)) {
			(*XW1) = (*X(I, NZM1))
		}
		if (*NZM2) > 0 && (*NZM2) <= (*(NUMNP)) {
			(*XW2) = (*X(I, NZM2))
		}
		XMN(I) = (intrinsic.MIN((*XW1), (*XW2)))
		XMX(I) = (intrinsic.MAX((*XW1), (*XW2)))
		DX(I) = XMX(I) - XMN(I)
		SX(I) = XMX(I) + XMN(I)
	}
	//!     Rescale window

	if (*DX(func() *int { y := 1; return &y }())) > (*DX(func() *int { y := 2; return &y }())) {
		XMN(func() *int { y := 2; return &y }()) = (SX(func() *int { y := 2; return &y }()) - DX(func() *int { y := 1; return &y }())) * 0.5e0
		XMX(func() *int { y := 2; return &y }()) = (SX(func() *int { y := 2; return &y }()) + DX(func() *int { y := 1; return &y }())) * 0.5e0
	} else {
		XMN(func() *int { y := 1; return &y }()) = (SX(func() *int { y := 1; return &y }()) - DX(func() *int { y := 2; return &y }())) * 0.5e0
		XMX(func() *int { y := 1; return &y }()) = (SX(func() *int { y := 1; return &y }()) + DX(func() *int { y := 2; return &y }())) * 0.5e0
	}
	for (*I) = 1; (*I) <= (*IJ); (*I)++ {
		XMIN(I) = (intrinsic.MAX(XMIN(I), XMN(I)))
		XMAX(I) = (intrinsic.MIN(XMAX(I), XMX(I)))
	}
	(*SCALEG) = (intrinsic.MAX(XMAX(func() *int { y := 1; return &y }())-XMIN(func() *int { y := 1; return &y }()), XMAX(func() *int { y := 2; return &y }())-XMIN(func() *int { y := 2; return &y }())))
	if (*II) > 2 {
		(*SCALEG) = (intrinsic.MAX((*SCALEG), XMAX(func() *int { y := 3; return &y }())-XMIN(func() *int { y := 3; return &y }())))
	}
	if (*SCALEG) <= 0.0e0 {
		(*SCALEG) = 1.e0
	}
	for (*I) = 1; (*I) <= (*IJ); (*I)++ {
		XMIN(I) = XMIN(I) - (*SCALEG)*0.01e0
		XMAX(I) = XMAX(I) + (*SCALEG)*0.01e0
	}
	//!     Default values

	(*SCALEF) = (intrinsic.MAX(XMAX(func() *int { y := 1; return &y }())-XMIN(func() *int { y := 1; return &y }()), XMAX(func() *int { y := 2; return &y }())-XMIN(func() *int { y := 2; return &y }())))
	if (*SCALEF) <= 0.0e0 {
		(*SCALEF) = (*SCALEG) * 0.1e0
	}
	//!     Reset values for deformed plotting

	//!     do i = 1,3

	for (*I) = 1; (*I) <= (*II); (*I)++ {
		(*XCEN) = XMAX(I) + XMIN(I)
		XMAX(I) = ((*XCEN) + 1.1e0*(*SCALEF)) * 0.5e0
		XMIN(I) = ((*XCEN) - 1.1e0*(*SCALEF)) * 0.5e0
	}
	(*SCALEG) = 0.4e0 / (*SCALEF)
	(*SCALEF) = (*SCALEG)
	S0(func() *int { y := 1; return &y }()) = 0.5e0
	S0(func() *int { y := 2; return &y }()) = 0.5e0
}
