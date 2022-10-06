package main

import "github.com/Konstantin8105/f4go/intrinsic"

type MEMORY struct {
	QUDSHPI struct {
		SA    *int
		GA    *int
		LA    *int
		LINT1 *int
		LINT2 *int
		LINT3 *int
	}
	QUDSHP6 struct {
		MATNP *[][]int
		PRJNP *[]float64
	}
	COMBLK struct {
		HR *float64
		MR *int
	}
	PDATA5 struct {
		EXORD *int
		EPORD *int
	}
	PLCLIP struct {
		IENTER *int
		IEXIT  *int
		FWIN   *bool
		CLCHK  *bool
		FWOFF  *bool
		PSOFF  *bool
		CMIN   *float64
		CMAX   *float64
		XC     *float64
		YC     *float64
	}
	QUDSHPN struct {
		S1W *float64
		S2W *float64
		S3W *float64
	}
	QUDSHP4 struct {
		SHPS2 *float64
	}
	QUDSHPM struct {
		SHPM *float64
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
	PPOINTS struct {
		PLIX *int64
	}
	IOFILE struct {
		KEEPFL *bool
		IOR    *int
		IOW    *int
	}
	PBODY struct {
		MAPLT *int
	}
	QUDSHP0 struct {
		JAC  *float64
		LINT *int
		NPM  *int
		NVN  *int
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
	PDATA6 struct {
		INORD *int
		IPORD *int
		IPU   *int
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
	QUDSHPA struct {
		QUAD   *bool
		TTFL   *bool
		NURBFL *bool
		VEMFL  *bool
	}
	QUDSHP5 struct {
		PHI   *[][]int
		PRESS *[]float64
		DVOL  *[]float64
	}
	SDATA struct {
		NDF  *int
		NDM  *int
		NEN1 *int
		NST  *int
		NNEQ *int
		NDL  *int
	}
}

var COMMON MEMORY

// !$Id:$
func PLFACN(IX *int, IA *int, NEN *int, NUMEL *int, NFACE *int, IE *int, NIE *int) {
	IOR := new(int)
	IOW := new(int)
	COMMON.IOFILE.IOW = new(float64)
	COMMON.IOFILE.IOR = new(int)
	KEEPFL := new(bool)
	COMMON.IOFILE.KEEPFL = new(int)
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
	CMIN := new(float64)
	CMAX := new(float64)
	XC := new(float64)
	YC := new(float64)
	IENTER := new(int)
	IEXIT := new(int)
	FWIN := new(bool)
	CLCHK := new(bool)
	FWOFF := new(bool)
	PSOFF := new(bool)
	COMMON.PLCLIP.YC = func() *[]float64 {
		arr := make([]float64, 4)
		return &arr
	}()
	COMMON.PLCLIP.XC = func() *[]float64 {
		arr := make([]float64, 4)
		return &arr
	}()
	COMMON.PLCLIP.CMAX(3) = new(float64)
	COMMON.PLCLIP.CMIN(3) = new(int)
	COMMON.PLCLIP.PSOFF = new(float64)
	COMMON.PLCLIP.FWOFF = new(float64)
	COMMON.PLCLIP.CLCHK = new(float64)
	COMMON.PLCLIP.FWIN = new(float64)
	COMMON.PLCLIP.IEXIT = new(float64)
	COMMON.PLCLIP.IENTER = new(int)
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
	I := new(int)
	J := new(int)
	M := new(int)
	N := new(int)
	IEL := new(int)
	IIEL := new(int)
	IEN := new(int)
	NEL := new(int)
	UFAC := new(int)
	PSTYP := new(int)
	IQ := new(int)
	IT := new(int)
	IT2 := new(int)
	//!      * * F E A P * * A Finite Element Analysis Program

	//!....  Copyright (c) 1984-2021: Regents of the University of California

	//!                               All rights reserved

	//!-----[--.----+----.----+----.-----------------------------------------]

	//!      Purpose: Determines which exterior faces are directed toward

	//!               view point.

	//!      Inputs:

	//!         ix(nen1,*)- Element nodal connection lists

	//!         ia(*)     - Active element plots based on materials

	//!         nen       - Number nodes/element

	//!         numel     - Number of elements/faces

	//!         ie(nie,*) - Material set assembly data

	//!         nie       - Dimension of ie array

	//!      Outputs:

	//!         nface     - Number of faces

	//!-----[--.----+----.----+----.-----------------------------------------]

	IOR = COMMON.IOFILE.IOR
	IOW = COMMON.IOFILE.IOW
	KEEPFL = COMMON.IOFILE.KEEPFL
	MAPLT = COMMON.PBODY.MAPLT
	EXORD = COMMON.PDATA5.EXORD
	EPORD = COMMON.PDATA5.EPORD
	INORD = COMMON.PDATA6.INORD
	IPORD = COMMON.PDATA6.IPORD
	IPU = COMMON.PDATA6.IPU
	CMIN = COMMON.PLCLIP.CMIN
	CMAX = COMMON.PLCLIP.CMAX
	XC = COMMON.PLCLIP.XC
	YC = COMMON.PLCLIP.YC
	IENTER = COMMON.PLCLIP.IENTER
	IEXIT = COMMON.PLCLIP.IEXIT
	FWIN = COMMON.PLCLIP.FWIN
	CLCHK = COMMON.PLCLIP.CLCHK
	FWOFF = COMMON.PLCLIP.FWOFF
	PSOFF = COMMON.PLCLIP.PSOFF
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

	//F4GO: NOT IMPLEMENTED :data IQ / 3 , 2 , 1 , 4 , 1 , 2 , 6 , 5 , 2 , 3 , 7 , 6 , 3 , 4 , 8 , 7 , 4 , 1 , 5 , 8 , 5 , 6 , 7 , 8 /
	//!     4-node tet faces

	//F4GO: NOT IMPLEMENTED :data IT / 1 , 2 , 4 , 2 , 3 , 4 , 3 , 1 , 4 , 1 , 3 , 2 /
	//!     10-node tet faces

	//F4GO: NOT IMPLEMENTED :data IT2 / 1 , 5 , 8 , 5 , 2 , 9 , 5 , 9 , 8 , 8 , 9 , 4 , 2 , 6 , 9 , 6 , 3 , 10 , 6 , 10 , 9 , 9 , 10 , 4 , 3 , 7 , 10 , 7 , 1 , 8 , 7 , 8 , 10 , 10 , 8 , 4 , 1 , 7 , 5 , 7 , 3 , 6 , 7 , 6 , 5 , 5 , 6 , 2 /
	//!     Compute location of boundary faces

	(*(NFACE)) = 0
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
				//!           No face if iiel < 0

				if (*IIEL) < 0 {
					//!           1-d elements

				} else if (*PSTYP) == 1 {
					//!             Set space for line elements

					if (*IIEL) > 0 && (*IIEL) <= 3 {
						if LCLIP((IX)(func() *int { y := 1; return &y }(), N), func() *int { y := 2; return &y }(), HR(NPXX), NDM) {
							(*(NFACE)) = (*(NFACE)) + 1
						}
					}
					//! iiel > 0

					//!           2-d elements

				} else if (*PSTYP) == 2 {
					//!             Set space for top and bottom faces

					if LCLIP((IX)(func() *int { y := 1; return &y }(), N), intrinsic.MIN(int(4), (*IEN)), HR(NPXX), NDM) {
						(*(NFACE)) = (*(NFACE)) + 2
					}
					//! iiel

					//!           3-d element plots

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

								if *ADDFAC {
									(*(NFACE)) = (*(NFACE)) + 1
								}
							}
							//! m

						}
						//!             Set for quadratic tetrahedral element faces

					} else if (*IIEL) == 15 {
						if LCLIP((IX)(func() *int { y := 1; return &y }(), N), func() *int { y := 4; return &y }(), HR(NPXX), NDM) {
							for (*M) = 1; (*M) <= 16; (*M)++ {
								(*ADDFAC) = true
								for (*J) = 1; (*J) <= 3; (*J)++ {
									(*I) = (IX)(IT2(J, M), N) - 1
									if (*MR((*NPRN) + (*I))) == 0 {
										(*ADDFAC) = false
									}
								}
								//! j

								if *ADDFAC {
									(*(NFACE)) = (*(NFACE)) + 1
								}
							}
							//! m

						}
						//!             64 node cubic brick

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

								if *ADDFAC {
									(*(NFACE)) = (*(NFACE)) + 1
								}
							}
							//! m

						}
						//!             Set for 8 to 27 node brick element faces

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

								if *ADDFAC {
									(*(NFACE)) = (*(NFACE)) + 1
								}
							}
							//! m

						}
					}
					//! iiel

				}
				//! pstyp > 0

				//!         User tests

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

						if *ADDFAC {
							(*(NFACE)) = (*(NFACE)) + 1
						}
					}
					//! m

				}
			}
			//! pstyp

		}
		//! pty

	}
	//! n

}
