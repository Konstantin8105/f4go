package main

import "github.com/Konstantin8105/f4go/intrinsic"

// !$Id:$
func MERGES(DIR *int, ZA *int, NIP *int, IP *int, NUMA *int, IC *int) {
	A := new(int)
	AU := new(int)
	B := new(int)
	BU := new(int)
	C := new(int)
	I := new(int)
	II := new(int)
	INC := new(int)
	STEP := new(int)
	NPASS := new(int)
	KIND := func() *[]float64 {
		arr := make([]float64, -1)
		return &arr
	}()
	//!      * * F E A P * * A Finite Element Analysis Program

	//!....  Copyright (c) 1984-2021: Regents of the University of California

	//!                               All rights reserved

	//!-----[--.----+----.----+----.-----------------------------------------]

	//!      Purpose: Merge sort routine

	//!      Inputs:

	//!          dir        - direction of sort

	//!                        > 0 for increasing sort;

	//!                        = 0 for increasing sort;

	//!                        < 0 for decreasing sort.

	//!          za(i)      - list of unsorted values

	//!          nip        - Dimension of ip array

	//!          ip(nip,i)  - list of pointer  values

	//!          numa       - number of items in list

	//!      Scratch:

	//!          ic(i)      - working array for sort

	//!                         ic must contain numa locations

	//!      Output:

	//!          za(i)      - unsorted list of values

	//!          ip(nip,i)  - sorted list of pointer values

	//!-----[--.----+----.----+----.-----------------------------------------]

	(*INC) = 1
	for (*II) = 1; (*II) <= (*(NUMA)); (*II)++ {
		(IC)(II) = 0
	}
	//! ii

	//!     Perform sort on list pairs in increments of step

	(*NPASS) = 0
Label100:
	;
	(*STEP) = (intrinsic.MIN(2*(*INC), intrinsic.MAX(1, (*(NUMA)))))
	for (*II) = 1; (*II) <= (*(NUMA)); (*II) += (*STEP) {
		//!       Set values for pointers - limit to entries in list

		(*A) = (*II)
		(*AU) = (intrinsic.MIN((*A)-1+(*INC), (*(NUMA))))
		(*B) = (intrinsic.MIN((*AU)+1, (*(NUMA))))
		(*BU) = (intrinsic.MIN((*B)-1+(*INC), (*(NUMA))))
		(*C) = 1
		//!       Perform merge of two lists

	Label110:
		;
		if (*(DIR)) >= 0 {
			//!         Increasing sort

			if (*ZA((IP)(func() *int { y := 1; return &y }(), A))) <= (*ZA((IP)(func() *int { y := 1; return &y }(), B))) {
				(IC)(C) = (*(IP)(func() *int { y := 1; return &y }(), A))
				(*A) = (*A) + 1
			} else {
				(IC)(C) = (*(IP)(func() *int { y := 1; return &y }(), B))
				(*B) = (*B) + 1
			}
		} else {
			//!         Decreasing sort

			if (*ZA((IP)(func() *int { y := 1; return &y }(), A))) >= (*ZA((IP)(func() *int { y := 1; return &y }(), B))) {
				(IC)(C) = (*(IP)(func() *int { y := 1; return &y }(), A))
				(*A) = (*A) + 1
			} else {
				(IC)(C) = (*(IP)(func() *int { y := 1; return &y }(), B))
				(*B) = (*B) + 1
			}
		}
		//!       Copy remaining list when first list is finished

		if (*A) > (*AU) {
			for (*I) = (*B); (*I) <= (*BU); (*I)++ {
				(*C) = (*C) + 1
				(IC)(C) = (*(IP)(func() *int { y := 1; return &y }(), I))
			}
			//! i

		} else if (*B) > (*BU) {
			for (*I) = (*A); (*I) <= (*AU); (*I)++ {
				(*C) = (*C) + 1
				(IC)(C) = (*(IP)(func() *int { y := 1; return &y }(), I))
			}
			//! i

			//!       Increment and repeat for next list items

		} else {
			(*C) = (*C) + 1
			goto Label110
			//  endif
		}
		//!       Lists have been sorted into ic, copy back to original list

		(*B) = (*II) - 1
		for (*I) = (*II); (*I) <= (*BU); (*I)++ {
			(IP)(func() *int { y := 1; return &y }(), I) = (*(IC)((*I) - (*B)))
		}
		//! i

	}
	//! ii

	//!     Increment step size for next pass

	(*INC) = (*STEP)
	(*NPASS) = (*NPASS) + 1
	if (*INC) < (*(NUMA)) {
		goto Label100
		//
	}
}
