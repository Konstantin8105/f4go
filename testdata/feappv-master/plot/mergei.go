package main

import "github.com/Konstantin8105/f4go/intrinsic"

// !$Id:$
func MERGEI(DIR *int, NIP *int, IP *int, NUMA *int, IPC *int) {
	A := new(int)
	AU := new(int)
	B := new(int)
	BU := new(int)
	C := new(int)
	I := new(int)
	II := new(int)
	INC := new(int)
	J := new(int)
	STEP := new(int)
	NPASS := new(int)
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

	//!          nip        - Dimension of ip array

	//!          ip(nip,i)  - list of pointer  values

	//!          numa       - number of items in list

	//!      Scratch:

	//!          ipc(nip,i)  - working array for sort

	//!                        ic must contain nip*numa locations

	//!      Output:

	//!          ip(nip,i)  - sorted list of pointer values

	//!-----[--.----+----.----+----.-----------------------------------------]

	(*INC) = 1
	for (*II) = 1; (*II) <= (*(NUMA)); (*II)++ {
		for (*J) = 1; (*J) <= (*(NIP)); (*J)++ {
			(IPC)(J, II) = 0
		}
		//! j

	}
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

			if (*(IP)(func() *int { y := 1; return &y }(), A)) <= (*(IP)(func() *int { y := 1; return &y }(), B)) {
				for (*J) = 1; (*J) <= (*(NIP)); (*J)++ {
					(IPC)(J, C) = (*(IP)(J, A))
				}
				//! j

				(*A) = (*A) + 1
			} else {
				for (*J) = 1; (*J) <= (*(NIP)); (*J)++ {
					(IPC)(J, C) = (*(IP)(J, B))
				}
				//! j

				(*B) = (*B) + 1
			}
		} else {
			//!         Decreasing sort

			if (*(IP)(func() *int { y := 1; return &y }(), A)) >= (*(IP)(func() *int { y := 1; return &y }(), B)) {
				for (*J) = 1; (*J) <= (*(NIP)); (*J)++ {
					(IPC)(J, C) = (*(IP)(J, A))
				}
				//! j

				(*A) = (*A) + 1
			} else {
				for (*J) = 1; (*J) <= (*(NIP)); (*J)++ {
					(IPC)(J, C) = (*(IP)(J, B))
				}
				//! j

				(*B) = (*B) + 1
			}
		}
		//!       Copy remaining list when first list is finished

		if (*A) > (*AU) {
			for (*I) = (*B); (*I) <= (*BU); (*I)++ {
				(*C) = (*C) + 1
				for (*J) = 1; (*J) <= (*(NIP)); (*J)++ {
					(IPC)(J, C) = (*(IP)(J, I))
				}
				//! j

			}
		} else if (*B) > (*BU) {
			for (*I) = (*A); (*I) <= (*AU); (*I)++ {
				(*C) = (*C) + 1
				for (*J) = 1; (*J) <= (*(NIP)); (*J)++ {
					(IPC)(J, C) = (*(IP)(J, I))
				}
				//! j

			}
			//!       Increment and repeat for next list items

		} else {
			(*C) = (*C) + 1
			goto Label110
			//  endif
		}
		//!       Lists have been sorted using ic, copy back to original list

		(*B) = (*II) - 1
		for (*I) = (*II); (*I) <= (*BU); (*I)++ {
			for (*J) = 1; (*J) <= (*(NIP)); (*J)++ {
				(IP)(J, I) = (*(IPC)(J, (*I)-(*B)))
			}
			//! j

		}
	}
	//!     Increment step size for next pass

	(*INC) = (*STEP)
	(*NPASS) = (*NPASS) + 1
	if (*INC) < (*(NUMA)) {
		goto Label100
		//
	}
}
