package main

import "github.com/Konstantin8105/f4go/intrinsic"

type MEMORY struct {
	PBODY struct {
		MAPLT *int
	}
	PDATA5 struct {
		EXORD *int
		EPORD *int
	}
	QUDSHPA struct {
		QUAD   *bool
		TTFL   *bool
		NURBFL *bool
		VEMFL  *bool
	}
	QUDSHPM struct {
		SHPM *float64
	}
	SDATA struct {
		NDF  *int
		NDM  *int
		NEN1 *int
		NST  *int
		NNEQ *int
		NDL  *int
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
	COMBLK struct {
		HR *float64
		MR *int
	}
	QUDHSIZ struct {
		HSIZE    *float64
		HKSIZE   *float64
		XX0      *float64
		DIST_MIN *float64
	}
	QUDSHPP struct {
		SIGP *[][]int
		EPSP *[][]float64
	}
	QUDSHPI struct {
		SA    *int
		GA    *int
		LA    *int
		LINT1 *int
		LINT2 *int
		LINT3 *int
	}
	QUDSHP5 struct {
		PHI   *[][]int
		PRESS *[]float64
		DVOL  *[]float64
	}
	PDATA6 struct {
		INORD *int
		IPORD *int
		IPU   *int
	}
	QUDSHP3 struct {
		SG3  *float64
		EL3  *float64
		SHP3 *float64
	}
	POINTER struct {
		NP *int64
		UP *int64
	}
	PPOINTS struct {
		PLIX *int64
	}
	QUDSHPN struct {
		S1W *float64
		S2W *float64
		S3W *float64
	}
	QUDSHP6 struct {
		MATNP *[][]int
		PRJNP *[]float64
	}
	QUDSHP0 struct {
		JAC  *float64
		LINT *int
		NPM  *int
		NVN  *int
	}
	QUDSHP1 struct {
		SG1  *float64
		SHP1 *float64
	}
	QUDSHP2 struct {
		SG2  *float64
		EL2  *float64
		SHP2 *float64
	}
	QUDSHP4 struct {
		SHPS2 *float64
	}
}

var COMMON MEMORY

// !$Id:$
func PLFACX(IX *int, IA *int, IXF *int, NEN *int, NUMEL *int, IE *int, NIE *int) {
	MAPLT := new(int)
	COMMON.PBODY.MAPLT = new(int)
	EXORD := new(int)
	EPORD := new(int)
	COMMON.PDATA5.EPORD = func() *[][]float64 {
		arr := make([][]float64, 50)
		for u := 0; u < 50; u++ {
			arr[u] = make([]float64, 50)
		}
		return &arr
	}()
	COMMON.PDATA5.EXORD = func() *[]int {
		arr := make([]int, 50)
		return &arr
	}()
	INORD := new(int)
	IPORD := new(int)
	IPU := new(int)
	COMMON.PDATA6.IPU = func() *[][]float64 {
		arr := make([][]float64, 4)
		for u := 0; u < 4; u++ {
			arr[u] = make([]float64, 200)
		}
		return &arr
	}()
	COMMON.PDATA6.IPORD = func() *[][]float64 {
		arr := make([][]float64, 200)
		for u := 0; u < 200; u++ {
			arr[u] = make([]float64, 50)
		}
		return &arr
	}()
	COMMON.PDATA6.INORD = func() *[]int {
		arr := make([]int, 50)
		return &arr
	}()
	QUAD := new(bool)
	TTFL := new(bool)
	NURBFL := new(bool)
	VEMFL := new(bool)
	COMMON.QUDSHPA.VEMFL = new(float64)
	COMMON.QUDSHPA.NURBFL = new(float64)
	COMMON.QUDSHPA.TTFL = new(float64)
	COMMON.QUDSHPA.QUAD = new(int)
	JAC := new(float64)
	LINT := new(int)
	NPM := new(int)
	NVN := new(int)
	COMMON.QUDSHP0.NVN = new(float64)
	COMMON.QUDSHP0.NPM = new(float64)
	COMMON.QUDSHP0.LINT = new(float64)
	COMMON.QUDSHP0.JAC = func() *[]int {
		arr := make([]int, 125)
		return &arr
	}()
	SG1 := new(float64)
	SHP1 := new(float64)
	COMMON.QUDSHP1.SHP1 = func() *[][][]float64 {
		arr := make([][][]float64, 2)
		for u := 0; u < 2; u++ {
			arr[u] = make([][]float64, 20)
			for w := 0; w < 20; w++ {
				arr[u][w] = make([]float64, 20)
			}
		}
		return &arr
	}()
	COMMON.QUDSHP1.SG1 = func() *[][]int {
		arr := make([][]int, 2)
		for u := 0; u < 2; u++ {
			arr[u] = make([]int, 20)
		}
		return &arr
	}()
	SG2 := new(float64)
	EL2 := new(float64)
	SHP2 := new(float64)
	COMMON.QUDSHP2.SHP2 = func() *[][][]float64 {
		arr := make([][][]float64, 3)
		for u := 0; u < 3; u++ {
			arr[u] = make([][]float64, 64)
			for w := 0; w < 64; w++ {
				arr[u][w] = make([]float64, 64)
			}
		}
		return &arr
	}()
	COMMON.QUDSHP2.EL2 = func() *[][]float64 {
		arr := make([][]float64, 4)
		for u := 0; u < 4; u++ {
			arr[u] = make([]float64, 16)
		}
		return &arr
	}()
	COMMON.QUDSHP2.SG2 = func() *[][]int {
		arr := make([][]int, 3)
		for u := 0; u < 3; u++ {
			arr[u] = make([]int, 64)
		}
		return &arr
	}()
	SG3 := new(float64)
	EL3 := new(float64)
	SHP3 := new(float64)
	COMMON.QUDSHP3.SHP3 = func() *[][][]float64 {
		arr := make([][][]float64, 4)
		for u := 0; u < 4; u++ {
			arr[u] = make([][]float64, 125)
			for w := 0; w < 125; w++ {
				arr[u][w] = make([]float64, 125)
			}
		}
		return &arr
	}()
	COMMON.QUDSHP3.EL3 = func() *[][]float64 {
		arr := make([][]float64, 5)
		for u := 0; u < 5; u++ {
			arr[u] = make([]float64, 16)
		}
		return &arr
	}()
	COMMON.QUDSHP3.SG3 = func() *[][]int {
		arr := make([][]int, 4)
		for u := 0; u < 4; u++ {
			arr[u] = make([]int, 125)
		}
		return &arr
	}()
	SHPS2 := new(float64)
	COMMON.QUDSHP4.SHPS2 = func() *[][][]int {
		arr := make([][][]int, 3)
		for u := 0; u < 3; u++ {
			arr[u] = make([][]int, 64)
			for w := 0; w < 64; w++ {
				arr[u][w] = make([]int, 64)
			}
		}
		return &arr
	}()
	S1W := new(float64)
	S2W := new(float64)
	S3W := new(float64)
	COMMON.QUDSHPN.S3W = func() *[][]float64 {
		arr := make([][]float64, 2)
		for u := 0; u < 2; u++ {
			arr[u] = make([]float64, 20)
		}
		return &arr
	}()
	COMMON.QUDSHPN.S2W = func() *[][]float64 {
		arr := make([][]float64, 2)
		for u := 0; u < 2; u++ {
			arr[u] = make([]float64, 20)
		}
		return &arr
	}()
	COMMON.QUDSHPN.S1W = func() *[][]int {
		arr := make([][]int, 2)
		for u := 0; u < 2; u++ {
			arr[u] = make([]int, 20)
		}
		return &arr
	}()
	SHPM := new(float64)
	COMMON.QUDSHPM.SHPM = func() *[][]int {
		arr := make([][]int, 125)
		for u := 0; u < 125; u++ {
			arr[u] = make([]int, 125)
		}
		return &arr
	}()
	HSIZE := new(float64)
	HKSIZE := new(float64)
	XX0 := new(float64)
	DIST_MIN := new(float64)
	COMMON.QUDHSIZ.DIST_MIN = new(float64)
	COMMON.QUDHSIZ.XX0 = func() *[]float64 {
		arr := make([]float64, 3)
		return &arr
	}()
	COMMON.QUDHSIZ.HKSIZE = func() *[]float64 {
		arr := make([]float64, 2)
		return &arr
	}()
	COMMON.QUDHSIZ.HSIZE = func() *[]int {
		arr := make([]int, 2)
		return &arr
	}()
	KIND := func() *[]float64 {
		arr := make([]float64, -1)
		return &arr
	}()
	EPSP := func() *[][]float64 {
		arr := make([][]float64, 6)
		for u := 0; u < 6; u++ {
			arr[u] = make([]float64, 125)
		}
		return &arr
	}()
	COMMON.QUDSHPP.EPSP = func() *[][]float64 {
		arr := make([][]float64, 6)
		for u := 0; u < 6; u++ {
			arr[u] = make([]float64, 125)
		}
		return &arr
	}()
	SIGP := func() *[][]int {
		arr := make([][]int, 10)
		for u := 0; u < 10; u++ {
			arr[u] = make([]int, 125)
		}
		return &arr
	}()
	COMMON.QUDSHPP.SIGP = func() *[][]int {
		arr := make([][]int, 10)
		for u := 0; u < 10; u++ {
			arr[u] = make([]int, 125)
		}
		return &arr
	}()
	SA := new(int)
	GA := new(int)
	LA := new(int)
	LINT1 := new(int)
	LINT2 := new(int)
	LINT3 := new(int)
	COMMON.QUDSHPI.LINT3 = new(float64)
	COMMON.QUDSHPI.LINT2 = new(float64)
	COMMON.QUDSHPI.LINT1 = new(float64)
	COMMON.QUDSHPI.LA = new(float64)
	COMMON.QUDSHPI.GA = new(float64)
	COMMON.QUDSHPI.SA = func() *[]int {
		arr := make([]int, 125)
		return &arr
	}()
	KIND := func() *[]float64 {
		arr := make([]float64, -1)
		return &arr
	}()
	COMMON.QUDSHP5.DVOL(125) = new(float64)
	PRESS := func() *[]float64 {
		arr := make([]float64, 125)
		return &arr
	}()
	COMMON.QUDSHP5.PRESS = func() *[]float64 {
		arr := make([]float64, 125)
		return &arr
	}()
	PHI := func() *[][]int {
		arr := make([][]int, 10)
		for u := 0; u < 10; u++ {
			arr[u] = make([]int, 125)
		}
		return &arr
	}()
	COMMON.QUDSHP5.PHI = func() *[][]int {
		arr := make([][]int, 10)
		for u := 0; u < 10; u++ {
			arr[u] = make([]int, 125)
		}
		return &arr
	}()
	KIND := func() *[]float64 {
		arr := make([]float64, -1)
		return &arr
	}()
	PRJNP := func() *[]float64 {
		arr := make([]float64, 125)
		return &arr
	}()
	COMMON.QUDSHP6.PRJNP = func() *[]float64 {
		arr := make([]float64, 125)
		return &arr
	}()
	MATNP := func() *[][]int {
		arr := make([][]int, 125)
		for u := 0; u < 125; u++ {
			arr[u] = make([]int, 125)
		}
		return &arr
	}()
	COMMON.QUDSHP6.MATNP = func() *[][]int {
		arr := make([][]int, 125)
		for u := 0; u < 125; u++ {
			arr[u] = make([]int, 125)
		}
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
	LCLIP := new(bool)
	ADDFAC := new(bool)
	SETVAL := new(bool)
	PALLOC := new(bool)
	I := new(int)
	J := new(int)
	J1 := new(int)
	M := new(int)
	N := new(int)
	NF := new(int)
	IEL := new(int)
	IIEL := new(int)
	IEN := new(int)
	NEL := new(int)
	UFAC := new(int)
	PSTYP := new(int)
	IQ := new(int)
	IT := new(int)
	IT2 := new(int)
	ILINE := new(int)
	II := new(int)
	//!      * * F E A P * * A Finite Element Analysis Program

	//!....  Copyright (c) 1984-2021: Regents of the University of California

	//!                               All rights reserved

	//!-----[--.----+----.----+----.-----------------------------------------]

	//!      Purpose: Construct surface mesh array - ixf

	//!      Inputs:

	//!         ix(nen1,*)- Element nodal connection lists

	//!         ia(*)     - Active element plots based on materials

	//!         nen       - Dimension of ix array

	//!         numel     - Number of elements

	//!         ie(nie,*) - Assembly data for material sets

	//!         nie       - Dimension of ie array

	//!      Outputs:

	//!         ixf(7,*)  - Face array

	//!-----[--.----+----.----+----.-----------------------------------------]

	MAPLT = COMMON.PBODY.MAPLT
	EXORD = COMMON.PDATA5.EXORD
	EPORD = COMMON.PDATA5.EPORD
	INORD = COMMON.PDATA6.INORD
	IPORD = COMMON.PDATA6.IPORD
	IPU = COMMON.PDATA6.IPU
	QUAD = COMMON.QUDSHPA.QUAD
	TTFL = COMMON.QUDSHPA.TTFL
	NURBFL = COMMON.QUDSHPA.NURBFL
	VEMFL = COMMON.QUDSHPA.VEMFL
	JAC = COMMON.QUDSHP0.JAC
	LINT = COMMON.QUDSHP0.LINT
	NPM = COMMON.QUDSHP0.NPM
	NVN = COMMON.QUDSHP0.NVN
	SG1 = COMMON.QUDSHP1.SG1
	SHP1 = COMMON.QUDSHP1.SHP1
	SG2 = COMMON.QUDSHP2.SG2
	EL2 = COMMON.QUDSHP2.EL2
	SHP2 = COMMON.QUDSHP2.SHP2
	SG3 = COMMON.QUDSHP3.SG3
	EL3 = COMMON.QUDSHP3.EL3
	SHP3 = COMMON.QUDSHP3.SHP3
	SHPS2 = COMMON.QUDSHP4.SHPS2
	S1W = COMMON.QUDSHPN.S1W
	S2W = COMMON.QUDSHPN.S2W
	S3W = COMMON.QUDSHPN.S3W
	SHPM = COMMON.QUDSHPM.SHPM
	HSIZE = COMMON.QUDHSIZ.HSIZE
	HKSIZE = COMMON.QUDHSIZ.HKSIZE
	XX0 = COMMON.QUDHSIZ.XX0
	DIST_MIN = COMMON.QUDHSIZ.DIST_MIN
	//F4GO: NOT IMPLEMENTED :real ( KIND = 8 ) :: SIGP , EPSP
	SIGP = COMMON.QUDSHPP.SIGP
	EPSP = COMMON.QUDSHPP.EPSP
	SA = COMMON.QUDSHPI.SA
	GA = COMMON.QUDSHPI.GA
	LA = COMMON.QUDSHPI.LA
	LINT1 = COMMON.QUDSHPI.LINT1
	LINT2 = COMMON.QUDSHPI.LINT2
	LINT3 = COMMON.QUDSHPI.LINT3
	//F4GO: NOT IMPLEMENTED :real ( KIND = 8 ) :: PHI , PRESS , DVOL
	PHI = COMMON.QUDSHP5.PHI
	PRESS = COMMON.QUDSHP5.PRESS
	DVOL = COMMON.QUDSHP5.DVOL
	//!     Projection arrays for local least-squares

	//F4GO: NOT IMPLEMENTED :real ( KIND = 8 ) :: MATNP , PRJNP
	MATNP = COMMON.QUDSHP6.MATNP
	PRJNP = COMMON.QUDSHP6.PRJNP
	NDF = COMMON.SDATA.NDF
	NDM = COMMON.SDATA.NDM
	NEN1 = COMMON.SDATA.NEN1
	NST = COMMON.SDATA.NST
	NNEQ = COMMON.SDATA.NNEQ
	NDL = COMMON.SDATA.NDL
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
	//!     8-node brick faces

	//F4GO: NOT IMPLEMENTED :data IQ / 3 , 2 , 1 , 4 , 1 , 2 , 6 , 5 , 2 , 3 , 7 , 6 , 3 , 4 , 8 , 7 , 4 , 1 , 5 , 8 , 5 , 6 , 7 , 8 , 1 , 2 , 3 , 4 /
	//!     4-node tet faces

	//F4GO: NOT IMPLEMENTED :data IT / 1 , 2 , 4 , 2 , 3 , 4 , 3 , 1 , 4 , 1 , 3 , 2 /
	//!     10-node tet faces

	//F4GO: NOT IMPLEMENTED :data IT2 / 1 , 5 , 8 , 5 , 2 , 9 , 5 , 9 , 8 , 8 , 9 , 4 , 2 , 6 , 9 , 6 , 3 , 10 , 6 , 10 , 9 , 9 , 10 , 4 , 3 , 7 , 10 , 7 , 1 , 8 , 7 , 8 , 10 , 10 , 8 , 4 , 1 , 7 , 5 , 7 , 3 , 6 , 7 , 6 , 5 , 5 , 6 , 2 /
	//!     Compute location of boundary faces

	(*NF) = 1
	for (*N) = 1; (*N) <= (*(NUMEL)); (*N)++ {
		if (IX)((*NEN1)-1, N) >= 0 && (IA)(N) >= 0 {
			(*PSTYP) = (*(IE)(func() *int { y := 1; return &y }(), (IX)(NEN1, N)))
			if (*PSTYP) > 0 {
				(*IEL) = (*(IE)((*(NIE))-1, (IX)(NEN1, N)))
				for (*J) = (*(NEN)); (*J) <= 1; (*J) += -1 {
					if (*(IX)(J, N)) > 0 {
						(*NEL) = (*J)
						(*EXIT)
					}
				}
				//! j

				//!           Get plot type

				PLFTYP(PSTYP, NEL, IEL)
				if (*IEL) > 0 {
					(*IIEL) = (*INORD(IEL))
				} else {
					(*IIEL) = (*EXORD(-(*IEL)))
				}
				//!           6-node triangle

				if (*IIEL) == 7 {
					(*IEN) = 3
				} else {
					(*IEN) = (*(NEN))
				}
				//!           No face if inord < 0

				if (*IIEL) < 0 {
					//!           1-d elements

				} else if (*PSTYP) == 1 {
					//!             Set space for line elements

					if (*IIEL) > 0 && (*IIEL) <= 3 {
						//!               Do a line element

						if LCLIP((IX)(func() *int { y := 1; return &y }(), N), func() *int { y := 2; return &y }(), HR(NPXX), NDM) {
							ILINE(func() *int { y := 1; return &y }()) = (*(IX)(func() *int { y := 1; return &y }(), N))
							ILINE(func() *int { y := 2; return &y }()) = (*(IX)(func() *int { y := 2; return &y }(), N))
							ILINE(func() *int { y := 3; return &y }()) = (*(IX)(func() *int { y := 2; return &y }(), N))
							ILINE(func() *int { y := 4; return &y }()) = (*(IX)(func() *int { y := 1; return &y }(), N))
							ILINE(func() *int { y := 5; return &y }()) = (*(IX)((*NEN1)-1, N))
							ILINE(func() *int { y := 6; return &y }()) = (*(IX)(NEN1, N))
							PFACEX(IQ(func() *int { y := 1; return &y }(), func() *int { y := 7; return &y }()), ILINE, (IXF)(func() *int { y := 1; return &y }(), NF), func() *int { y := 4; return &y }(), func() *int { y := 6; return &y }(), NF, N)
						}
					}
					//! iiel > 0

					//!           2-d elements

				} else if (*PSTYP) == 2 {
					//!             Set space for top and bottom surface faces

					if LCLIP((IX)(func() *int { y := 1; return &y }(), N), intrinsic.MIN(int(4), (*IEN)), HR(NPXX), NDM) {
						//!               Do a 2-d surface with both faces considered

						II(func() *int { y := 1; return &y }()) = 1
						for (*I) = 2; (*I) <= (intrinsic.MIN(int(4), (*IEN))); (*I)++ {
							if (*(IX)(IQ(I, func() *int { y := 1; return &y }()), N)) > 0 {
								if (*(IX)(IQ(II(func() *int { y := 1; return &y }()), func() *int { y := 1; return &y }()), N)) > (*(IX)(IQ(I, func() *int { y := 1; return &y }()), N)) {
									II(func() *int { y := 1; return &y }()) = (*I)
								}
							}
						}
						//! i

						II(func() *int { y := 2; return &y }()) = intrinsic.MOD(II(func() *int { y := 1; return &y }()), intrinsic.MIN(int(4), (*IEN))) + 1
						II(func() *int { y := 3; return &y }()) = intrinsic.MOD(II(func() *int { y := 2; return &y }()), intrinsic.MIN(int(4), (*IEN))) + 1
						II(func() *int { y := 4; return &y }()) = intrinsic.MOD(II(func() *int { y := 3; return &y }()), intrinsic.MIN(int(4), (*IEN))) + 1
						ILINE(func() *int { y := 4; return &y }()) = 0
						for (*J) = 1; (*J) <= (intrinsic.MIN(int(4), (*IEN))); (*J)++ {
							ILINE(J) = (*(IX)(IQ(II(J), func() *int { y := 1; return &y }()), N))
						}
						//! j

						ILINE(func() *int { y := 5; return &y }()) = (*(IX)((*NEN1)-1, N))
						ILINE(func() *int { y := 6; return &y }()) = (*(IX)(NEN1, N))
						PFACEX(IQ(func() *int { y := 1; return &y }(), func() *int { y := 7; return &y }()), ILINE, (IXF)(func() *int { y := 1; return &y }(), NF), intrinsic.MIN((*IEN), int(4)), func() *int { y := 6; return &y }(), NF, -(*N))
						II(func() *int { y := 1; return &y }()) = 1
						for (*I) = 2; (*I) <= (intrinsic.MIN(int(4), (*IEN))); (*I)++ {
							if (*(IX)(IQ(I, func() *int { y := 7; return &y }()), N)) > 0 {
								if (*(IX)(IQ(II(func() *int { y := 1; return &y }()), func() *int { y := 7; return &y }()), N)) > (*(IX)(IQ(I, func() *int { y := 7; return &y }()), N)) {
									II(func() *int { y := 1; return &y }()) = (*I)
								}
							}
						}
						//! i

						II(func() *int { y := 2; return &y }()) = intrinsic.MOD(II(func() *int { y := 1; return &y }()), intrinsic.MIN(int(4), (*IEN))) + 1
						II(func() *int { y := 3; return &y }()) = intrinsic.MOD(II(func() *int { y := 2; return &y }()), intrinsic.MIN(int(4), (*IEN))) + 1
						II(func() *int { y := 4; return &y }()) = intrinsic.MOD(II(func() *int { y := 3; return &y }()), intrinsic.MIN(int(4), (*IEN))) + 1
						for (*J) = 1; (*J) <= (intrinsic.MIN(int(4), (*IEN))); (*J)++ {
							ILINE(J) = (*(IX)(IQ(II(J), func() *int { y := 7; return &y }()), N))
						}
						//! j

						ILINE(func() *int { y := 5; return &y }()) = (*(IX)((*NEN1)-1, N))
						ILINE(func() *int { y := 6; return &y }()) = (*(IX)(NEN1, N))
						PFACEX(IQ(func() *int { y := 1; return &y }(), func() *int { y := 7; return &y }()), ILINE, (IXF)(func() *int { y := 1; return &y }(), NF), intrinsic.MIN((*IEN), int(4)), func() *int { y := 6; return &y }(), NF, -(*N))
					}
					//!           3-d elements

				} else if (*PSTYP) == 3 {
					//!             Set for linear tetrahedral element faces

					if (*IIEL) == 9 {
						if LCLIP((IX)(func() *int { y := 1; return &y }(), N), func() *int { y := 4; return &y }(), HR(NPXX), NDM) {
							for (*M) = 1; (*M) <= 4; (*M)++ {
								(*ADDFAC) = true
								for (*J) = 1; (*J) <= 3; (*J)++ {
									(*I) = (IX)(IT(J, M), N) - 1
									if (*MR((*NPRN) + (*I))) == 0 {
										(*ADDFAC) = false
									}
								}
								//! j

								//!                   Face is to be plotted if visible

								if *ADDFAC {
									II(func() *int { y := 1; return &y }()) = 1
									for (*I) = 2; (*I) <= 3; (*I)++ {
										if (*(IX)(IT(II(func() *int { y := 1; return &y }()), M), N)) > (*(IX)(IT(I, M), N)) {
											II(func() *int { y := 1; return &y }()) = (*I)
										}
									}
									//! i

									II(func() *int { y := 2; return &y }()) = intrinsic.MOD(II(func() *int { y := 1; return &y }()), int(3)) + 1
									II(func() *int { y := 3; return &y }()) = intrinsic.MOD(II(func() *int { y := 2; return &y }()), int(3)) + 1
									ILINE(func() *int { y := 1; return &y }()) = (*(IX)(IT(II(func() *int { y := 1; return &y }()), M), N))
									ILINE(func() *int { y := 2; return &y }()) = (*(IX)(IT(II(func() *int { y := 2; return &y }()), M), N))
									ILINE(func() *int { y := 3; return &y }()) = (*(IX)(IT(II(func() *int { y := 3; return &y }()), M), N))
									ILINE(func() *int { y := 4; return &y }()) = 0
									ILINE(func() *int { y := 5; return &y }()) = (*(IX)((*NEN1)-1, N))
									ILINE(func() *int { y := 6; return &y }()) = (*(IX)(NEN1, N))
									PFACEX(IQ(func() *int { y := 1; return &y }(), func() *int { y := 7; return &y }()), ILINE, (IXF)(func() *int { y := 1; return &y }(), NF), func() *int { y := 4; return &y }(), func() *int { y := 6; return &y }(), NF, N)
								}
							}
							//! m

						}
						//!             Set for quadratic tetrahedral element faces

					} else if (*IIEL) == 15 {
						if LCLIP((IX)(func() *int { y := 1; return &y }(), N), func() *int { y := 10; return &y }(), HR(NPXX), NDM) {
							for (*M) = 1; (*M) <= 16; (*M)++ {
								(*ADDFAC) = true
								for (*J) = 1; (*J) <= 3; (*J)++ {
									(*I) = (IX)(IT2(J, M), N) - 1
									if (*MR((*NPRN) + (*I))) == 0 {
										(*ADDFAC) = false
									}
								}
								//! j

								//!                   Face is to be plotted if visible

								if *ADDFAC {
									II(func() *int { y := 1; return &y }()) = 1
									for (*I) = 2; (*I) <= 3; (*I)++ {
										if (*(IX)(IT2(II(func() *int { y := 1; return &y }()), M), N)) > (*(IX)(IT2(I, M), N)) {
											II(func() *int { y := 1; return &y }()) = (*I)
										}
									}
									//! i

									II(func() *int { y := 2; return &y }()) = intrinsic.MOD(II(func() *int { y := 1; return &y }()), int(3)) + 1
									II(func() *int { y := 3; return &y }()) = intrinsic.MOD(II(func() *int { y := 2; return &y }()), int(3)) + 1
									ILINE(func() *int { y := 1; return &y }()) = (*(IX)(IT2(II(func() *int { y := 1; return &y }()), M), N))
									ILINE(func() *int { y := 2; return &y }()) = (*(IX)(IT2(II(func() *int { y := 2; return &y }()), M), N))
									ILINE(func() *int { y := 3; return &y }()) = (*(IX)(IT2(II(func() *int { y := 3; return &y }()), M), N))
									ILINE(func() *int { y := 4; return &y }()) = 0
									ILINE(func() *int { y := 5; return &y }()) = (*(IX)((*NEN1)-1, N))
									ILINE(func() *int { y := 6; return &y }()) = (*(IX)(NEN1, N))
									PFACEX(IQ(func() *int { y := 1; return &y }(), func() *int { y := 7; return &y }()), ILINE, (IXF)(func() *int { y := 1; return &y }(), NF), func() *int { y := 4; return &y }(), func() *int { y := 6; return &y }(), NF, N)
								}
							}
							//! m

						}
						//!             Set for 64-node brick element faces

					} else if (*IIEL) == 46 && !(*NURBFL) {
						if LCLIP((IX)(func() *int { y := 1; return &y }(), N), (NEN), HR(NPXX), NDM) {
							PFACEPQR(func() *int { y := 3; return &y }(), IPU, UFAC)
							for (*M) = 1; (*M) <= (*UFAC); (*M)++ {
								(*ADDFAC) = true
								for (*J) = 1; (*J) <= 4; (*J)++ {
									(*I) = (IX)(IPU(J, M), N) - 1
									if (*MR((*NPRN) + (*I))) == 0 {
										(*ADDFAC) = false
									}
								}
								//! j

								//!                   Face is to be plotted if visible

								if *ADDFAC {
									for (*J) = 1; (*J) <= 4; (*J)++ {
										ILINE(J) = (*(IX)(IPU(J, M), N))
									}
									//! j

									ILINE(func() *int { y := 5; return &y }()) = (*(IX)((*NEN1)-1, N))
									ILINE(func() *int { y := 6; return &y }()) = (*(IX)(NEN1, N))
									PFACEX(IQ(func() *int { y := 1; return &y }(), func() *int { y := 7; return &y }()), ILINE, (IXF)(func() *int { y := 1; return &y }(), NF), func() *int { y := 4; return &y }(), func() *int { y := 6; return &y }(), NF, N)
								}
							}
							//! m

						}
						//!             Set for brick element faces

					} else if (*IIEL) > 10 {
						if LCLIP((IX)(func() *int { y := 1; return &y }(), N), func() *int { y := 8; return &y }(), HR(NPXX), NDM) {
							for (*M) = 1; (*M) <= 6; (*M)++ {
								(*ADDFAC) = true
								for (*J) = 1; (*J) <= 4; (*J)++ {
									(*I) = (IX)(IQ(J, M), N) - 1
									if (*MR((*NPRN) + (*I))) == 0 {
										(*ADDFAC) = false
									}
								}
								//! j

								//!                   Face is to be plotted if visible

								if *ADDFAC {
									II(func() *int { y := 1; return &y }()) = 1
									for (*I) = 2; (*I) <= 4; (*I)++ {
										if (*(IX)(IQ(II(func() *int { y := 1; return &y }()), M), N)) > (*(IX)(IQ(I, M), N)) {
											II(func() *int { y := 1; return &y }()) = (*I)
										}
									}
									//! i

									II(func() *int { y := 2; return &y }()) = intrinsic.MOD(II(func() *int { y := 1; return &y }()), int(4)) + 1
									II(func() *int { y := 3; return &y }()) = intrinsic.MOD(II(func() *int { y := 2; return &y }()), int(4)) + 1
									II(func() *int { y := 4; return &y }()) = intrinsic.MOD(II(func() *int { y := 3; return &y }()), int(4)) + 1
									for (*J) = 1; (*J) <= 4; (*J)++ {
										ILINE(J) = (*(IX)(IQ(II(J), M), N))
									}
									//! j

									ILINE(func() *int { y := 5; return &y }()) = (*(IX)((*NEN1)-1, N))
									ILINE(func() *int { y := 6; return &y }()) = (*(IX)(NEN1, N))
									PFACEX(IQ(func() *int { y := 1; return &y }(), func() *int { y := 7; return &y }()), ILINE, (IXF)(func() *int { y := 1; return &y }(), NF), func() *int { y := 4; return &y }(), func() *int { y := 6; return &y }(), NF, N)
								}
							}
							//! m

						}
					}
					//! iiel

				}
				//! pstyp > 0

				//!         User element test

			} else if (*PSTYP) < 0 {
				if LCLIP((IX)(func() *int { y := 1; return &y }(), N), (NEN), HR(NPXX), NDM) {
					UFACELIB(PSTYP, NEL, IPU, UFAC)
					for (*M) = 1; (*M) <= (*UFAC); (*M)++ {
						(*ADDFAC) = true
						for (*J) = 1; (*J) <= 4; (*J)++ {
							(*I) = (IX)(IPU(J, M), N) - 1
							if (*MR((*NPRN) + (*I))) == 0 {
								(*ADDFAC) = false
							}
						}
						//! j

						//!               Face is to be plotted if visible

						if *ADDFAC {
							for (*J) = 1; (*J) <= 4; (*J)++ {
								ILINE(J) = (*(IX)(IPU(J, M), N))
							}
							//! j

							ILINE(func() *int { y := 5; return &y }()) = (*(IX)((*NEN1)-1, N))
							ILINE(func() *int { y := 6; return &y }()) = (*(IX)(NEN1, N))
							PFACEX(IQ(func() *int { y := 1; return &y }(), func() *int { y := 7; return &y }()), ILINE, (IXF)(func() *int { y := 1; return &y }(), NF), func() *int { y := 4; return &y }(), func() *int { y := 6; return &y }(), NF, N)
						}
					}
					//! m

				}
			}
			//! pstyp

		}
	}
	//! n

	//!     Sort faces

	(*NF) = (*NF) - 1
	if (*NF) > 1 {
		(*SETVAL) = (*PALLOC(func() *int { y := 118; return &y }(), func() *[]byte { y := []byte("TEMP8"); return &y }(), 7*(*NF), func() *int { y := 1; return &y }()))
		MERGEI(func() *int { y := 0; return &y }(), func() *int { y := 7; return &y }(), (IXF), NF, MR(NP(func() *int { y := 118; return &y }())))
		(*SETVAL) = (*PALLOC(func() *int { y := 118; return &y }(), func() *[]byte { y := []byte("TEMP8"); return &y }(), func() *int { y := 0; return &y }(), func() *int { y := 1; return &y }()))
	}
	//!     Search for matching faces and delete

	(*I) = 1
	II(func() *int { y := 1; return &y }()) = (*(IXF)(func() *int { y := 1; return &y }(), I))
	for (*J) = 2; (*J) <= (*NF); (*J)++ {
		if (IXF)(func() *int { y := 1; return &y }(), J) > II(func() *int { y := 1; return &y }()) || (*J) == (*NF) {
			if (*J) == (*NF) {
				(*J1) = (*NF)
			} else {
				(*J1) = (*J) - 1
			}
			for (*M) = (*I); (*M) <= (*J1); (*M)++ {
				if (IXF)(func() *int { y := 4; return &y }(), M) == 0 || (IXF)(func() *int { y := 4; return &y }(), M) == (IXF)(func() *int { y := 1; return &y }(), M) {
					for (*N) = (*M) + 1; (*N) <= (*J1); (*N)++ {
						if (IXF)(func() *int { y := 2; return &y }(), M) == (IXF)(func() *int { y := 3; return &y }(), N) && (IXF)(func() *int { y := 3; return &y }(), M) == (IXF)(func() *int { y := 2; return &y }(), N) && ((IXF)(func() *int { y := 4; return &y }(), N) == (IXF)(func() *int { y := 1; return &y }(), N) || (IXF)(func() *int { y := 4; return &y }(), N) == 0) && (IXF)(func() *int { y := 5; return &y }(), M) > 0 {
							(IXF)(func() *int { y := 7; return &y }(), M) = -intrinsic.ABS((IXF)(func() *int { y := 7; return &y }(), M))
							(IXF)(func() *int { y := 7; return &y }(), N) = -intrinsic.ABS((IXF)(func() *int { y := 7; return &y }(), N))
						}
					}
					//! n

					(IXF)(func() *int { y := 4; return &y }(), M) = (*(IXF)(func() *int { y := 1; return &y }(), M))
				} else {
					for (*N) = (*M) + 1; (*N) <= (*J1); (*N)++ {
						if (IXF)(func() *int { y := 2; return &y }(), M) == (IXF)(func() *int { y := 4; return &y }(), N) && (IXF)(func() *int { y := 4; return &y }(), M) == (IXF)(func() *int { y := 2; return &y }(), N) && (IXF)(func() *int { y := 3; return &y }(), M) == (IXF)(func() *int { y := 3; return &y }(), N) && (IXF)(func() *int { y := 5; return &y }(), M) > 0 {
							(IXF)(func() *int { y := 7; return &y }(), M) = -intrinsic.ABS((IXF)(func() *int { y := 7; return &y }(), M))
							(IXF)(func() *int { y := 7; return &y }(), N) = -intrinsic.ABS((IXF)(func() *int { y := 7; return &y }(), N))
						}
					}
					//! n

				}
			}
			//! m

			(*I) = (*J)
			II(func() *int { y := 1; return &y }()) = (*(IXF)(func() *int { y := 1; return &y }(), J))
		}
	}
	//! j

}
