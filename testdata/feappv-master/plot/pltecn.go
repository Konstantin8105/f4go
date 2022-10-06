package main

import "github.com/Konstantin8105/f4go/intrinsic"

type MEMORY struct {
	PDATA2 struct {
		ICLEAR *int
		IDEV   *int
		IDX    *int
		IDY    *int
		IPB    *int
		IFRM   *int
		ICOLR  *int
		ILNO   *int
	}
	PDATRI struct {
		XMX *float64
		XMN *float64
		YMX *float64
		YMN *float64
	}
	PLCON1 struct {
		VLU   *float64
		DX1   *float64
		NLABI *int
		VFLG  *bool
		TVC   *bool
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
}

var COMMON MEMORY

// !$Id:$
func PLTECN(XT *int, VT *int, VC *int, NC *int) {
	ICLEAR := new(int)
	IDEV := new(int)
	IDX := new(int)
	IDY := new(int)
	IPB := new(int)
	IFRM := new(int)
	ICOLR := new(int)
	ILNO := new(int)
	COMMON.PDATA2.ILNO = func() *[]float64 {
		arr := make([]float64, 2)
		return &arr
	}()
	COMMON.PDATA2.ICOLR = new(float64)
	COMMON.PDATA2.IFRM = new(float64)
	COMMON.PDATA2.IPB = new(float64)
	COMMON.PDATA2.IDY = new(float64)
	COMMON.PDATA2.IDX = new(float64)
	COMMON.PDATA2.IDEV = new(float64)
	COMMON.PDATA2.ICLEAR = new(int)
	XMX := new(float64)
	XMN := new(float64)
	YMX := new(float64)
	YMN := new(float64)
	COMMON.PDATRI.YMN = new(float64)
	COMMON.PDATRI.YMX = new(float64)
	COMMON.PDATRI.XMN = new(float64)
	COMMON.PDATRI.XMX = new(int)
	VLU := new(float64)
	DX1 := new(float64)
	NLABI := new(int)
	VFLG := new(bool)
	TVC := new(bool)
	COMMON.PLCON1.TVC = func() *[][]float64 {
		arr := make([][]float64, 9)
		for u := 0; u < 9; u++ {
			arr[u] = make([]float64, 9)
		}
		return &arr
	}()
	COMMON.PLCON1.VFLG = new(float64)
	COMMON.PLCON1.NLABI = new(float64)
	COMMON.PLCON1.DX1 = new(float64)
	COMMON.PLCON1.VLU = func() *[]int {
		arr := make([]int, 2)
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
	I := new(int)
	J := new(int)
	II := new(int)
	IVC := new(int)
	JVC := new(int)
	NN := new(int)
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

	//!      Purpose: Plot contours/fills for 2-d surface elements.

	//!               N.B. All facets are divided into triangle for plots

	//!      Inputs:

	//!         xt(3,3)   - Triangle coordinates

	//!         vt(3)     - Triangle vertex values

	//!         vc(*)     - Contour values

	//!         nc        - Number of contours

	//!      Outputs:

	//!         none      - Plot outputs to screen/file

	//!-----[--.----+----.----+----.-----------------------------------------]

	ICLEAR = COMMON.PDATA2.ICLEAR
	IDEV = COMMON.PDATA2.IDEV
	IDX = COMMON.PDATA2.IDX
	IDY = COMMON.PDATA2.IDY
	IPB = COMMON.PDATA2.IPB
	IFRM = COMMON.PDATA2.IFRM
	ICOLR = COMMON.PDATA2.ICOLR
	ILNO = COMMON.PDATA2.ILNO
	XMX = COMMON.PDATRI.XMX
	XMN = COMMON.PDATRI.XMN
	YMX = COMMON.PDATRI.YMX
	YMN = COMMON.PDATRI.YMN
	VLU = COMMON.PLCON1.VLU
	DX1 = COMMON.PLCON1.DX1
	NLABI = COMMON.PLCON1.NLABI
	VFLG = COMMON.PLCON1.VFLG
	TVC = COMMON.PLCON1.TVC
	BORDFL = COMMON.PLFLAG.BORDFL
	SCREFL = COMMON.PLFLAG.SCREFL
	EVERON = COMMON.PLFLAG.EVERON
	BLK = COMMON.PLFLAG.BLK
	CLIP = COMMON.PLFLAG.CLIP
	LSTRK = COMMON.PLFLAG.LSTRK
	LFILL = COMMON.PLFLAG.LFILL
	PLOUT = COMMON.PLFLAG.PLOUT
	//F4GO: NOT IMPLEMENTED :real ( KIND = 8 ) :: S , X1 , Y1 , Z1 , VV
	//F4GO: NOT IMPLEMENTED :real ( KIND = 8 ) :: VC ( 12 ) , VT ( 3 ) , XT ( 3 , 3 )
	//!     Plot all contours which intersect element

	for (*NN) = 1; (*NN) <= (*(NC)); (*NN)++ {
		(*VV) = (*VC(NN))
		if (*VV) >= VLU(func() *int { y := 1; return &y }()) && (*VV) <= VLU(func() *int { y := 2; return &y }()) {
			PPPCOL((*NLABI)+(*NN), func() *int { y := 1; return &y }())
			//!         Loop over sides of triangle to find plot points

			(*J) = 3
			for (*I) = 1; (*I) <= 3; (*I)++ {
				(*II) = intrinsic.MOD((*I), int(3)) + 1
				if (*VV) == (*VT(I)) {
					(*X1) = (*XT(func() *int { y := 1; return &y }(), I))
					(*Y1) = (*XT(func() *int { y := 2; return &y }(), I))
					(*Z1) = (*XT(func() *int { y := 3; return &y }(), I))
					PLOTL(X1, Y1, Z1, J)
					(*J) = 2
				} else if (VT(I)-(*VV))*(VT(II)-(*VV)) < 0.0e0 {
					(*S) = ((*VV) - VT(I)) / (VT(II) - VT(I))
					(*X1) = XT(func() *int { y := 1; return &y }(), I) + (*S)*(XT(func() *int { y := 1; return &y }(), II)-XT(func() *int { y := 1; return &y }(), I))
					(*Y1) = XT(func() *int { y := 2; return &y }(), I) + (*S)*(XT(func() *int { y := 2; return &y }(), II)-XT(func() *int { y := 2; return &y }(), I))
					(*Z1) = XT(func() *int { y := 3; return &y }(), I) + (*S)*(XT(func() *int { y := 3; return &y }(), II)-XT(func() *int { y := 3; return &y }(), I))
					PLOTL(X1, Y1, Z1, J)
					(*J) = 2
				}
				//!           Add labels

				if (*VFLG) && (*J) == 2 {
					(*IVC) = (intrinsic.MAX(1, intrinsic.MIN(int(9), NINT(((*X1)-(*XMN))*(*XMX)+1))))
					(*JVC) = (intrinsic.MAX(1, intrinsic.MIN(int(9), NINT(((*Y1)-(*YMN))*(*YMX)+1))))
					if TVC(IVC, JVC) {
						TVC(IVC, JVC) = false
						PLOTL((*X1)-(*DX1), Y1, Z1, func() *int { y := 3; return &y }())
						if *CLIP {
							PLABL((*NLABI) + (*NN))
						}
						PLOTL(X1, Y1, Z1, func() *int { y := 3; return &y }())
					}
				}
			}
			//! i

		}
	}
	//! nn

}
