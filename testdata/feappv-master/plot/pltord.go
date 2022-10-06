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
	PDATA3 struct {
		NPSTR *int
		PLFL  *bool
		HIDE  *bool
	}
	PDATA5 struct {
		EXORD *int
		EPORD *int
	}
	PDATA6 struct {
		INORD *int
		IPORD *int
		IPU   *int
	}
}

var COMMON MEMORY

// !$Id:$
func PLTORD(IX *int, IEL *int, IJU *int, JPLT *int) {
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
	IIU := new(int)
	IJ := new(int)
	IPLT := new(int)
	//!      * * F E A P * * A Finite Element Analysis Program

	//!....  Copyright (c) 1984-2021: Regents of the University of California

	//!                               All rights reserved

	//!-----[--.----+----.----+----.-----------------------------------------]

	//!      Purpose: Sets plot order for element type iel

	//!      Inputs:

	//!         ix(*)     - Nodal connection list

	//!         iel       - Element type

	//!      Outputs:

	//!         iju       - Number of points to describe element plot

	//!         jplt(*)   - Element nodal plot sequence

	//!-----[--.----+----.----+----.-----------------------------------------]

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
	EXORD = COMMON.PDATA5.EXORD
	EPORD = COMMON.PDATA5.EPORD
	INORD = COMMON.PDATA6.INORD
	IPORD = COMMON.PDATA6.IPORD
	IPU = COMMON.PDATA6.IPU
	//!     Default orders for 3-9 node 2-d elements

	//F4GO: NOT IMPLEMENTED :data IPLT / 1 , 5 , 2 , 6 , 3 , 7 , 4 , 8 , 1 /
	//!     Get number of plot points to go around element

	(*(IJU)) = 0
	if (*(IEL)) > 0 {
		(*IIU) = (*INORD((IEL)))
		if (*IIU) != 0 && !(*HIDE) {
			//!         Set plot table for specified values

			for (*IJ) = 1; (*IJ) <= (intrinsic.ABS((*IIU))); (*IJ)++ {
				if IPORD(IJ, (IEL)) > 0 && IPORD(IJ, (IEL)) <= (*NEN) {
					if (*(IX)(IPORD(IJ, (IEL)))) > 0 {
						(*(IJU)) = (*(IJU)) + 1
						(JPLT)((IJU)) = (*IPORD(IJ, (IEL)))
					}
				}
			}
			//! ij

		} else if *HIDE {
			//!         Set plot table for hidden surface

			if (*IIU) > 0 && (*IIU) < 5 {
				for (*IJ) = 1; (*IJ) <= (intrinsic.ABS((*IIU))); (*IJ)++ {
					if IPORD(IJ, (IEL)) > 0 && IPORD(IJ, (IEL)) <= (*NEN) {
						if (*(IX)(IPORD(IJ, (IEL)))) > 0 {
							(*(IJU)) = (*(IJU)) + 1
							(JPLT)((IJU)) = (*IPORD(IJ, (IEL)))
						}
					}
				}
				//! ij

			} else {
				//!           Set plot table for 4 node element

				for (*IJ) = 1; (*IJ) <= 4; (*IJ)++ {
					if (*(IX)(IJ)) > 0 {
						(*(IJU)) = (*(IJU)) + 1
						(JPLT)((IJU)) = (*IJ)
					}
				}
				//! ij

				(*(IJU)) = (*(IJU)) + 1
				(JPLT)((IJU)) = 1
			}
		} else if (*NEN) == 3 {
			//!         Set plot table for 3 node element

			for (*IJ) = 1; (*IJ) <= 3; (*IJ)++ {
				if (*(IX)(IJ)) > 0 {
					(*(IJU)) = (*(IJU)) + 1
					(JPLT)((IJU)) = (*IJ)
				}
			}
			//! ij

			(*(IJU)) = (*(IJU)) + 1
			(JPLT)((IJU)) = 1
		} else {
			//!       Set plot table for 3-9 node element

			for (*IJ) = 1; (*IJ) <= 9; (*IJ)++ {
				if (*IPLT(IJ)) <= (*NEN) {
					if (*(IX)(IPLT(IJ))) > 0 {
						(*(IJU)) = (*(IJU)) + 1
						(JPLT)((IJU)) = (*IPLT(IJ))
					}
				}
			}
			//! ij

		}
	} else if (*(IEL)) < 0 {
		(*IIU) = (*EXORD(-(*(IEL))))
		if (*IIU) != 0 && !(*HIDE) {
			//!         Set plot table for specified values

			for (*IJ) = 1; (*IJ) <= (intrinsic.ABS((*IIU))); (*IJ)++ {
				if EPORD(IJ, -(*(IEL))) > 0 && EPORD(IJ, -(*(IEL))) <= (*NEN) {
					if (*(IX)(EPORD(IJ, -(*(IEL))))) > 0 {
						(*(IJU)) = (*(IJU)) + 1
						(JPLT)((IJU)) = (*EPORD(IJ, -(*(IEL))))
					}
				}
			}
			//! ij

		} else if *HIDE {
			//!         Set plot table for hidden surface

			if (*IIU) > 0 && (*IIU) < 5 {
				for (*IJ) = 1; (*IJ) <= (intrinsic.ABS((*IIU))); (*IJ)++ {
					if EPORD(IJ, -(*(IEL))) > 0 && EPORD(IJ, -(*(IEL))) <= (*NEN) {
						if (*(IX)(EPORD(IJ, -(*(IEL))))) > 0 {
							(*(IJU)) = (*(IJU)) + 1
							(JPLT)((IJU)) = (*EPORD(IJ, -(*(IEL))))
						}
					}
				}
				//! ij

			} else {
				//!           Set plot table for 4 node element

				for (*IJ) = 1; (*IJ) <= 4; (*IJ)++ {
					if (*(IX)(IJ)) > 0 {
						(*(IJU)) = (*(IJU)) + 1
						(JPLT)((IJU)) = (*IJ)
					}
				}
				//! ij

				(*(IJU)) = (*(IJU)) + 1
				(JPLT)((IJU)) = 1
			}
		} else if (*NEN) == 3 {
			//!         Set plot table for 3 node element

			for (*IJ) = 1; (*IJ) <= 3; (*IJ)++ {
				if (*(IX)(IJ)) > 0 {
					(*(IJU)) = (*(IJU)) + 1
					(JPLT)((IJU)) = (*IJ)
				}
			}
			//! ij

			(*(IJU)) = (*(IJU)) + 1
			(JPLT)((IJU)) = 1
		} else {
			//!       Set plot table for 3-9 node element

			for (*IJ) = 1; (*IJ) <= 9; (*IJ)++ {
				if (*IPLT(IJ)) <= (*NEN) {
					if (*(IX)(IPLT(IJ))) > 0 {
						(*(IJU)) = (*(IJU)) + 1
						(JPLT)((IJU)) = (*IPLT(IJ))
					}
				}
			}
			//! ij

		}
	}
}
