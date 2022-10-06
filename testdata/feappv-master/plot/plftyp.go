package main

// !$Id:$
func PLFTYP(PSTYP *int, NEL *int, IEL *int) {
	//!      * * F E A P * * A Finite Element Analysis Program

	//!....  Copyright (c) 1984-2021: Regents of the University of California

	//!                               All rights reserved

	//!-----[--.----+----.----+----.-----------------------------------------]

	//!      Purpose: Set the plot for this element

	//!      Inputs:

	//!         pstyp   - Element topology

	//!         nel     - Number of element nodes

	//!         iel     - Element number

	//!      Output:

	//!         inord   - Number of plot

	//!-----[--.----+----.----+----.-----------------------------------------]

	//!      One dimensional plot sets

	if (*(PSTYP)) == 1 {
		if (*(NEL)) == 1 {
			PLTPT1((IEL))
		} else if (*(NEL)) == 3 {
			PLTLN3((IEL))
		} else {
			PLTLN2((IEL))
		}
		//!      Two dimensional plot sets

	} else if (*(PSTYP)) == 2 {
		if (*(NEL)) == 3 {
			PLTRI3((IEL))
		} else if (*(NEL)) == 6 || (*(NEL)) == 7 {
			PLTRI6((IEL))
		} else if (*(NEL)) == 8 || (*(NEL)) == 9 {
			PLQUD8((IEL))
		} else if (*(NEL)) == 16 {
			PLTQ16((IEL))
		} else {
			PLQUD4((IEL))
		}
		//!      Three dimensional plot sets

	} else if (*(PSTYP)) == 3 {
		if (*(NEL)) == 4 {
			PLTET4((IEL))
		} else if (*(NEL)) == 10 || (*(NEL)) == 11 {
			PLTET10((IEL))
		} else if (*(NEL)) == 14 || (*(NEL)) == 15 {
			PLTET10((IEL))
		} else if (*(NEL)) == 20 || (*(NEL)) == 27 {
			PLBK27((IEL))
		} else if (*(NEL)) == 64 {
			PLBKPQR(func() *int { y := 3; return &y }(), (IEL))
		} else {
			PLBRK8((IEL))
		}
		//!      User plots

	} else if (*(PSTYP)) < 0 {
		UFTYPLIB((PSTYP), (NEL), (IEL))
	}
}
