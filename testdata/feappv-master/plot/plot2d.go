package main

import "github.com/Konstantin8105/f4go/intrinsic"

type MEMORY struct {
	ELDATA struct {
		DM    *float64
		N_EL  *int
		MA    *int
		MCT   *int
		IEL   *int
		NEL   *int
		PSTYP *int
		ELTYP *int
		ELTY2 *int
		ELTY3 *int
	}
	PBODY struct {
		MAPLT *int
	}
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
}

var COMMON MEMORY

// !$Id:$
func PLOT2D(IE *int, IX *int, IP *int, X *int, XL *int, NIE *int, NDM *int, NEN *int, NEN1 *int, NUME *int, N1 *int, N2 *int) {
	DM := new(float64)
	N_EL := new(int)
	MA := new(int)
	MCT := new(int)
	IEL := new(int)
	NEL := new(int)
	PSTYP := new(int)
	ELTYP := new(int)
	ELTY2 := new(int)
	ELTY3 := new(int)
	COMMON.ELDATA.ELTY3 = new(float64)
	COMMON.ELDATA.ELTY2 = new(float64)
	COMMON.ELDATA.ELTYP = new(float64)
	COMMON.ELDATA.PSTYP = new(float64)
	COMMON.ELDATA.NEL = new(float64)
	COMMON.ELDATA.IEL = new(float64)
	COMMON.ELDATA.MCT = new(float64)
	COMMON.ELDATA.MA = new(float64)
	COMMON.ELDATA.N_EL = new(float64)
	COMMON.ELDATA.DM = new(int)
	MAPLT := new(int)
	COMMON.PBODY.MAPLT = new(int)
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
	ISP := new(int)
	I := new(int)
	J := new(int)
	II := new(int)
	N := new(int)
	NN := new(int)
	KIND := func() *[]float64 {
		arr := make([]float64, -1)
		return &arr
	}()
	//!      * * F E A P * * A Finite Element Analysis Program

	//!....  Copyright (c) 1984-2021: Regents of the University of California

	//!                               All rights reserved

	//!-----[--.----+----.----+----.-----------------------------------------]

	//!      Purpose: Two dimensional mesh plot routine

	//!      Inputs:

	//!         ie(nie,*) - Assembly data for material sets

	//!         ix(nen1,*)- Element nodal connections

	//!         ip(*)     - Sorted element order

	//!         x(ndm,*)  - Nodal coordinates

	//!         ndm       - Dimension of x  array

	//!         nen       - Number of nodes on element

	//!         nen1      - Dimension of ix array

	//!         nume      - Number of elements/faces

	//!         n1        - Color for plots

	//!         n2        - Outline indicator

	//!      Scratch:

	//!         xl(ndm,*) - Element nodal coordinates

	//!      Outputs:

	//!         none      - Plot output to screen/file

	//!-----[--.----+----.----+----.-----------------------------------------]

	DM = COMMON.ELDATA.DM
	N_EL = COMMON.ELDATA.N_EL
	MA = COMMON.ELDATA.MA
	MCT = COMMON.ELDATA.MCT
	IEL = COMMON.ELDATA.IEL
	NEL = COMMON.ELDATA.NEL
	PSTYP = COMMON.ELDATA.PSTYP
	ELTYP = COMMON.ELDATA.ELTYP
	ELTY2 = COMMON.ELDATA.ELTY2
	ELTY3 = COMMON.ELDATA.ELTY3
	MAPLT = COMMON.PBODY.MAPLT
	ICLEAR = COMMON.PDATA2.ICLEAR
	IDEV = COMMON.PDATA2.IDEV
	IDX = COMMON.PDATA2.IDX
	IDY = COMMON.PDATA2.IDY
	IPB = COMMON.PDATA2.IPB
	IFRM = COMMON.PDATA2.IFRM
	ICOLR = COMMON.PDATA2.ICOLR
	ILNO = COMMON.PDATA2.ILNO
	//F4GO: NOT IMPLEMENTED :real ( KIND = 8 ) :: X ( NDM , * ) , XL ( NDM , * )
	//!     Loop over elements to draw mesh

	for (*NN) = 1; (*NN) <= (*(NUME)); (*NN)++ {
		(*N) = (*(IP)(NN))
		if (*N) > 0 {
			if (*(IX)((*(NEN1))-1, N)) >= 0 {
				(*MA) = (*(IX)((NEN1), N))
				//!           Plot correct material number: ma > 0 active material

				if (*MAPLT) == 0 || (*MA) == (*MAPLT) {
					if (*(N1)) == 0 {
						(*ICOLR) = (*MA) + 1
					} else if (*(N1)) < 0 {
						(*ICOLR) = intrinsic.MOD((IX)((*(NEN1))-1, N), int(7)) + 1
					} else {
						(*ICOLR) = (*(N1))
					}
					PPPCOL(ICOLR, func() *int { y := 0; return &y }())
					for (*I) = 1; (*I) <= (*(NEN)); (*I)++ {
						(*II) = (intrinsic.ABS((IX)(I, N)))
						if (*II) > 0 {
							(*NEL) = (*I)
							for (*J) = 1; (*J) <= (*(NDM)); (*J)++ {
								XL(J, I) = (*X(J, II))
							}
						} else {
							for (*J) = 1; (*J) <= (*(NDM)); (*J)++ {
								XL(J, I) = 0.0e0
							}
						}
					}
					//!             Check for a line element

					if (IX)(func() *int { y := 1; return &y }(), N) == (IX)(func() *int { y := 4; return &y }(), N) && (IX)(func() *int { y := 2; return &y }(), N) == (IX)(func() *int { y := 3; return &y }(), N) {
						(*NEL) = 2
					}
					if (*(N2)) > 0 {
						(*ISP) = -1
					} else {
						(*ISP) = 1
					}
					PLOT9((IE)((*(NIE))-1, MA), (IX)(func() *int { y := 1; return &y }(), N), XL, (NDM), NEL, ISP)
				}
			}
		}
	}
}
