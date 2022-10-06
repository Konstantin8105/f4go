package main

type MEMORY struct {
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
	SDATA struct {
		NDF  *int
		NDM  *int
		NEN1 *int
		NST  *int
		NNEQ *int
		NDL  *int
	}
	CDATA struct {
		NUMNP  *int
		NUMEL  *int
		NUMMAT *int
		NEN    *int
		NEQ    *int
		IPR    *int
		NETYP  *int
	}
	PLFLAG struct {
		PLOUT  *bool
		BORDFL *bool
		SCREFL *bool
		EVERON *bool
		BLK    *bool
		CLIP   *bool
		LSTRK  *bool
		LFILL  *bool
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
	COMBLK struct {
		HR *float64
		MR *int
	}
	PDATA3 struct {
		NPSTR *int
		PLFL  *bool
		HIDE  *bool
	}
}

var COMMON MEMORY

// !$Id:$
func PHIDE(CT *int, NIX *int, NXD *int, NXN *int, NNE *int, NFACE *int, ILN *int) {
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
	NPSTR := new(int)
	PLFL := new(bool)
	HIDE := new(bool)
	COMMON.PDATA3.HIDE = new(float64)
	COMMON.PDATA3.PLFL = new(float64)
	COMMON.PDATA3.NPSTR = new(int)
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
	BORDFL := new(bool)
	SCREFL := new(bool)
	EVERON := new(bool)
	BLK := new(bool)
	CLIP := new(bool)
	LSTRK := new(bool)
	LFILL := new(bool)
	COMMON.PLFLAG.LFILL = new(float64)
	COMMON.PLFLAG.LSTRK = new(float64)
	COMMON.PLFLAG.CLIP = new(float64)
	COMMON.PLFLAG.BLK = new(float64)
	COMMON.PLFLAG.EVERON = new(float64)
	COMMON.PLFLAG.SCREFL = new(float64)
	COMMON.PLFLAG.BORDFL = new(int)
	PLOUT := new(bool)
	COMMON.PLFLAG.PLOUT = new(int)
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
	KIND := func() *[]float64 {
		arr := make([]float64, -1)
		return &arr
	}()
	//!      * * F E A P * * A Finite Element Analysis Program

	//!....  Copyright (c) 1984-2021: Regents of the University of California

	//!                               All rights reserved

	//!     P l o t   C o n t r o l   R o u t i n e   F o r   F E A P

	//!-----[--.----+----.----+----.-----------------------------------------]

	//!      Purpose: do hidden line removal

	//!      Inputs:

	//!         ct        - Plot negative faces if positive

	//!         iln(2)    - Line type data

	//!         nface     - Number of faces on surfaces

	//!      Outputs:

	//!         nix       - Face connection location

	//!         nxd       - Face connection dimension

	//!         nxn       - Number of nodes/face

	//!         nne       - Number of faces

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
	NPSTR = COMMON.PDATA3.NPSTR
	PLFL = COMMON.PDATA3.PLFL
	HIDE = COMMON.PDATA3.HIDE
	XMIN = COMMON.PDATA4.XMIN
	XMAX = COMMON.PDATA4.XMAX
	NZM1 = COMMON.PDATA4.NZM1
	NZM2 = COMMON.PDATA4.NZM2
	NFAC = COMMON.PDATA4.NFAC
	NFMX = COMMON.PDATA4.NFMX
	PDF = COMMON.PDATA4.PDF
	BORDFL = COMMON.PLFLAG.BORDFL
	SCREFL = COMMON.PLFLAG.SCREFL
	EVERON = COMMON.PLFLAG.EVERON
	BLK = COMMON.PLFLAG.BLK
	CLIP = COMMON.PLFLAG.CLIP
	LSTRK = COMMON.PLFLAG.LSTRK
	LFILL = COMMON.PLFLAG.LFILL
	PLOUT = COMMON.PLFLAG.PLOUT
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
	E = COMMON.PPERS.E
	EOLD = COMMON.PPERS.EOLD
	TG = COMMON.PPERS.TG
	VOLD = COMMON.PPERS.VOLD
	Q = COMMON.PPERS.Q
	XLBDA = COMMON.PPERS.XLBDA
	ENORM = COMMON.PPERS.ENORM
	KPERS = COMMON.PPERS.KPERS
	NDF = COMMON.SDATA.NDF
	NDM = COMMON.SDATA.NDM
	NEN1 = COMMON.SDATA.NEN1
	NST = COMMON.SDATA.NST
	NNEQ = COMMON.SDATA.NNEQ
	NDL = COMMON.SDATA.NDL
	//!     Plot visible mesh

	PZEROI(MR(NP(func() *int { y := 66; return &y }())), NUMNP)
	(*(NIX)) = 54
	(*(NXD)) = 7
	(*(NXN)) = 4
	(*(NNE)) = (*(NFACE))
	(*NFAC) = (*(NNE))
	PLFACE(MR(NP((NIX))), MR(NP(func() *int { y := 62; return &y }())), HR(NP(func() *int { y := 53; return &y }())), func() *int { y := 3; return &y }(), (NXD), NUMNP, (NNE), (ILN), CT)
	//!     Set plot sequence for z-sort

	if (*KPERS) != 0 {
		PERSPZ(HR(NP(func() *int { y := 53; return &y }())), MR(NP((NIX))), MR(NP(func() *int { y := 62; return &y }())), (NXD), (NXN), func() *int { y := 3; return &y }(), NUMNP, (NNE))
	}
}
