package main

import "github.com/Konstantin8105/f4go/intrinsic"

type MEMORY struct {
	CDATA struct {
		NUMNP  *int
		NUMEL  *int
		NUMMAT *int
		NEN    *int
		NEQ    *int
		IPR    *int
		NETYP  *int
	}
	SDATA struct {
		NDF  *int
		NDM  *int
		NEN1 *int
		NST  *int
		NNEQ *int
		NDL  *int
	}
	COMBLK struct {
		HR *float64
		MR *int
	}
	HDATAM struct {
		PLTMFL *bool
		NHMAX  *int
		NH3MAX *int
		HFLGU  *bool
		H3FLGU *bool
		NDFLG  *bool
	}
	PDATA3 struct {
		NPSTR *int
		PLFL  *bool
		HIDE  *bool
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
	STRNUM struct {
		ISTV *int
		ISTE *int
		ISTP *int
		ISTC *int
	}
}

var COMMON MEMORY

// !$Id:$
func PJSTRS(TRIFL *bool) {
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
	NUMNP := new(int)
	NUMEL := new(int)
	NUMMAT := new(int)
	NEN := new(int)
	NEQ := new(int)
	IPR := new(int)
	NETYP := new(int)
	COMMON.CDATA.NETYP = new(float64)
	COMMON.CDATA.IPR = new(float64)
	COMMON.CDATA.NEQ = new(float64)
	COMMON.CDATA.NEN = new(float64)
	COMMON.CDATA.NUMMAT = new(float64)
	COMMON.CDATA.NUMEL = new(float64)
	COMMON.CDATA.NUMNP = new(int)
	NHMAX := new(int)
	NH3MAX := new(int)
	HFLGU := new(bool)
	H3FLGU := new(bool)
	NDFLG := new(bool)
	COMMON.HDATAM.NDFLG = new(float64)
	COMMON.HDATAM.H3FLGU = new(float64)
	COMMON.HDATAM.HFLGU = new(float64)
	COMMON.HDATAM.NH3MAX = new(float64)
	COMMON.HDATAM.NHMAX = new(int)
	PLTMFL := new(bool)
	COMMON.HDATAM.PLTMFL = new(int)
	NPSTR := new(int)
	PLFL := new(bool)
	HIDE := new(bool)
	COMMON.PDATA3.HIDE = new(float64)
	COMMON.PDATA3.PLFL = new(float64)
	COMMON.PDATA3.NPSTR = new(int)
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
	NDF := new(int)
	NDM := new(int)
	NEN1 := new(int)
	NST := new(int)
	NNEQ := new(int)
	NDL := new(int)
	COMMON.SDATA.NDL = new(float64)
	COMMON.SDATA.NNEQ = new(float64)
	COMMON.SDATA.NST = new(float64)
	COMMON.SDATA.NEN1 = new(float64)
	COMMON.SDATA.NDM = new(float64)
	COMMON.SDATA.NDF = new(int)
	ISTV := new(int)
	ISTE := new(int)
	ISTP := new(int)
	ISTC := new(int)
	COMMON.STRNUM.ISTC = new(float64)
	COMMON.STRNUM.ISTP = new(float64)
	COMMON.STRNUM.ISTE = new(float64)
	COMMON.STRNUM.ISTV = new(int)
	I := new(int)
	II := new(int)
	//!      * * F E A P * * A Finite Element Analysis Program

	//!....  Copyright (c) 1984-2021: Regents of the University of California

	//!                               All rights reserved

	//!-----[--.----+----.----+----.-----------------------------------------]

	//!      Purpose: Project nodal stresses

	//!      Inputs:

	//!         trifl      - Flag, generate element size for tri2d if true

	//!      Outputs:

	//!         none       - Output stored in blank common arrays

	//!-----[--.----+----.----+----.-----------------------------------------]

	HR = COMMON.COMBLK.HR
	MR = COMMON.COMBLK.MR
	NUMNP = COMMON.CDATA.NUMNP
	NUMEL = COMMON.CDATA.NUMEL
	NUMMAT = COMMON.CDATA.NUMMAT
	NEN = COMMON.CDATA.NEN
	NEQ = COMMON.CDATA.NEQ
	IPR = COMMON.CDATA.IPR
	NETYP = COMMON.CDATA.NETYP
	NHMAX = COMMON.HDATAM.NHMAX
	NH3MAX = COMMON.HDATAM.NH3MAX
	HFLGU = COMMON.HDATAM.HFLGU
	H3FLGU = COMMON.HDATAM.H3FLGU
	NDFLG = COMMON.HDATAM.NDFLG
	PLTMFL = COMMON.HDATAM.PLTMFL
	NPSTR = COMMON.PDATA3.NPSTR
	PLFL = COMMON.PDATA3.PLFL
	HIDE = COMMON.PDATA3.HIDE
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
	NDF = COMMON.SDATA.NDF
	NDM = COMMON.SDATA.NDM
	NEN1 = COMMON.SDATA.NEN1
	NST = COMMON.SDATA.NST
	NNEQ = COMMON.SDATA.NNEQ
	NDL = COMMON.SDATA.NDL
	ISTV = COMMON.STRNUM.ISTV
	ISTE = COMMON.STRNUM.ISTE
	ISTP = COMMON.STRNUM.ISTP
	ISTC = COMMON.STRNUM.ISTC
	//!     Stress projections

	(*ISTV) = (*NPSTR) - 1
	(*II) = 0
	for (*I) = (*NEN1) - 1; (*I) <= (*NEN1)*(*NUMEL)-1; (*I) += (*NEN1) {
		if (*MR(NP(func() *int { y := 128; return &y }()) + (*II))) < 0 {
			MR(NP(func() *int { y := 33; return &y }()) + (*I)) = -intrinsic.ABS(MR(NP(func() *int { y := 33; return &y }()) + (*I)))
		}
		(*II) = (*II) + 1
	}
	//! i

	PZERO(HR(NPNP), (*NPSTR)*(*NUMNP))
	PZERO(HR(NPER), 8*(*NUMNP))
	if !(*(TRIFL)) {
		PZERO(HR(NP(func() *int { y := 207; return &y }())), NUMEL)
	}
	(*PLTMFL) = true
	FORMFE(NP(func() *int { y := 40; return &y }()), NP(func() *int { y := 26; return &y }()), NP(func() *int { y := 26; return &y }()), NP(func() *int { y := 26; return &y }()), &(false), &(false), &(false), func() *int { y := 8; return &y }(), func() *int { y := 1; return &y }(), NUMEL, func() *int { y := 1; return &y }())
	(*PLTMFL) = false
	PLTSTR(HR(NPNP), HR((*NPER)+(*NUMNP)), HR((*NPNP)+(*NUMNP)), NUMNP, NDM)
	for (*I) = (*NEN1) - 1; (*I) <= (*NEN1)*(*NUMEL)-1; (*I) += (*NEN1) {
		MR(NP(func() *int { y := 33; return &y }()) + (*I)) = (intrinsic.ABS(MR(NP(func() *int { y := 33; return &y }()) + (*I))))
	}
	//! i

}
