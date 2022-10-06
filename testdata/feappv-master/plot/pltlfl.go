package main

import "github.com/Konstantin8105/f4go/intrinsic"

// !$Id:$
func PLTLFL(NS *int, XL *int, V *int, VC *int, NC *int) {
	ICOL := new(int)
	ICX := new(int)
	ICN := new(int)
	I := new(int)
	J := new(int)
	K := new(int)
	KIND := func() *[]float64 {
		arr := make([]float64, -1)
		return &arr
	}()
	ILC := new(int)
	KIND := func() *[]float64 {
		arr := make([]float64, -1)
		return &arr
	}()
	IPAL := func() *[]int {
		arr := make([]int, 7)
		return &arr
	}()
	//!      * * F E A P * * A Finite Element Analysis Program

	//!....  Copyright (c) 1984-2021: Regents of the University of California

	//!                               All rights reserved

	//!-----[--.----+----.----+----.-----------------------------------------]

	//!      Purpose: Plot of mesh contours: With inter-element smoothing

	//!      Inputs:

	//!         ns       - Number of segments

	//!         xl(3,*)  - Nodal coordinates of line segment

	//!         v(*)     - Nodal solution values

	//!         vc(*)    - Contour values

	//!         nc       - Number of contour intervals

	//!      Outputs:

	//!         none      - Plot outputs to screen/file

	//!-----[--.----+----.----+----.-----------------------------------------]

	//F4GO: NOT IMPLEMENTED :real ( KIND = 8 ) :: S , VCI
	//F4GO: NOT IMPLEMENTED :real ( KIND = 8 ) :: XL ( 3 , * ) , V ( * ) , VC ( * ) , XP ( 9 ) , YP ( 9 ) , ZP ( 9 )
	//!     Color pallet

	(*IPAL)[1-(1)], (*IPAL)[2-(1)], (*IPAL)[3-(1)], (*IPAL)[4-(1)], (*IPAL)[5-(1)], (*IPAL)[6-(1)], (*IPAL)[7-(1)] = 1, 6, 4, 5, 3, 7, 2
	PLTCOR((*(NS))+1, ILC, V, VC, (NC))
	VC((*(NC)) + 1) = (intrinsic.MAX(VC(func() *int { y := 1; return &y }()), VC((NC))))
	(*VCI) = 0.0e0
	(*ICX) = (*ILC(func() *int { y := 1; return &y }()))
	(*ICN) = (*ILC(func() *int { y := 1; return &y }()))
	for (*I) = 1; (*I) <= (*(NS)); (*I)++ {
		(*VCI) = (intrinsic.MAX((*VCI), intrinsic.ABS(V(I))))
		VC((*(NC)) + 1) = (intrinsic.MAX(VC((*(NC))+1), V(I)))
		(*ICX) = (intrinsic.MAX(ILC(I), (*ICX)))
		(*ICN) = (intrinsic.MIN(ILC(I), (*ICN)))
	}
	VC((*(NC)) + 1) = VC((*(NC))+1)*1.001 + (*VCI) + 1.0e-8
	for (*ICOL) = (*ICN); (*ICOL) <= (*ICX); (*ICOL)++ {
		(*I) = (*IPAL)[(*ICOL)-(1)]
		PPPCOL(I, func() *int { y := 0; return &y }())
		(*K) = 0
		(*I) = (*(NS))
		for (*J) = 1; (*J) <= (*(NS)); (*J)++ {
			if (ILC(J) >= (*ICOL) && ILC(I) <= (*ICOL)) && (ILC(I) != ILC(J)) {
				if (*ICOL)-1 >= (*ILC(I)) {
					(*S) = (VC((*ICOL)-1) - V(I)) / (V(J) - V(I))
					(*K) = (*K) + 1
					XP(K) = XL(func() *int { y := 1; return &y }(), I) + (XL(func() *int { y := 1; return &y }(), J)-XL(func() *int { y := 1; return &y }(), I))*(*S)
					YP(K) = XL(func() *int { y := 2; return &y }(), I) + (XL(func() *int { y := 2; return &y }(), J)-XL(func() *int { y := 2; return &y }(), I))*(*S)
					ZP(K) = XL(func() *int { y := 3; return &y }(), I) + (XL(func() *int { y := 3; return &y }(), J)-XL(func() *int { y := 3; return &y }(), I))*(*S)
				}
				(*S) = (VC(ICOL) - V(I)) / (V(J) - V(I))
				if (*S) < 1.0e0 {
					(*K) = (*K) + 1
					XP(K) = XL(func() *int { y := 1; return &y }(), I) + (XL(func() *int { y := 1; return &y }(), J)-XL(func() *int { y := 1; return &y }(), I))*(*S)
					YP(K) = XL(func() *int { y := 2; return &y }(), I) + (XL(func() *int { y := 2; return &y }(), J)-XL(func() *int { y := 2; return &y }(), I))*(*S)
					ZP(K) = XL(func() *int { y := 3; return &y }(), I) + (XL(func() *int { y := 3; return &y }(), J)-XL(func() *int { y := 3; return &y }(), I))*(*S)
				}
			} else if (ILC(I) >= (*ICOL) && ILC(J) <= (*ICOL)) && (ILC(I) != ILC(J)) {
				(*S) = (VC(ICOL) - V(I)) / (V(J) - V(I))
				if (*S) >= 0.0e0 {
					(*K) = (*K) + 1
					XP(K) = XL(func() *int { y := 1; return &y }(), I) + (XL(func() *int { y := 1; return &y }(), J)-XL(func() *int { y := 1; return &y }(), I))*(*S)
					YP(K) = XL(func() *int { y := 2; return &y }(), I) + (XL(func() *int { y := 2; return &y }(), J)-XL(func() *int { y := 2; return &y }(), I))*(*S)
					ZP(K) = XL(func() *int { y := 3; return &y }(), I) + (XL(func() *int { y := 3; return &y }(), J)-XL(func() *int { y := 3; return &y }(), I))*(*S)
				}
				if (*ICOL)-1 >= (*ILC(J)) {
					(*S) = (VC((*ICOL)-1) - V(I)) / (V(J) - V(I))
					(*K) = (*K) + 1
					XP(K) = XL(func() *int { y := 1; return &y }(), I) + (XL(func() *int { y := 1; return &y }(), J)-XL(func() *int { y := 1; return &y }(), I))*(*S)
					YP(K) = XL(func() *int { y := 2; return &y }(), I) + (XL(func() *int { y := 2; return &y }(), J)-XL(func() *int { y := 2; return &y }(), I))*(*S)
					ZP(K) = XL(func() *int { y := 3; return &y }(), I) + (XL(func() *int { y := 3; return &y }(), J)-XL(func() *int { y := 3; return &y }(), I))*(*S)
				}
			}
			if (*ILC(J)) == (*ICOL) {
				(*K) = (*K) + 1
				XP(K) = (*XL(func() *int { y := 1; return &y }(), J))
				YP(K) = (*XL(func() *int { y := 2; return &y }(), J))
				ZP(K) = (*XL(func() *int { y := 3; return &y }(), J))
			}
			(*I) = (*J)
		}
		//!       Plot line of this color

		PLOTL(XP(func() *int { y := 1; return &y }()), YP(func() *int { y := 1; return &y }()), ZP(func() *int { y := 1; return &y }()), func() *int { y := 3; return &y }())
		for (*J) = 2; (*J) <= (*K); (*J)++ {
			PLOTL(XP(J), YP(J), ZP(J), func() *int { y := 2; return &y }())
		}
		PPPCOL(func() *int { y := 0; return &y }(), func() *int { y := 0; return &y }())
	}
}
