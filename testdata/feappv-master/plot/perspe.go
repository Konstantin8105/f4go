package main

import "github.com/Konstantin8105/f4go/intrinsic"

type MEMORY struct {
	IOFILE struct {
		KEEPFL *bool
		IOR    *int
		IOW    *int
	}
	PDATA0 struct {
		VMIN *float64
		VMAX *float64
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
	PRMPTD struct {
		PROMPT *bool
		DEFALT *bool
		PSCOLR *bool
		PSREVS *bool
	}
	PDATPS struct {
		HDCPY  *bool
		HDLOGO *bool
		PSFRAM *bool
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
func PERSPE(FLAG *bool) {
	IOR := new(int)
	IOW := new(int)
	COMMON.IOFILE.IOW = new(float64)
	COMMON.IOFILE.IOR = new(int)
	KEEPFL := new(bool)
	COMMON.IOFILE.KEEPFL = new(int)
	VMIN := new(float64)
	VMAX := new(float64)
	COMMON.PDATA0.VMAX = func() *[]float64 {
		arr := make([]float64, 3)
		return &arr
	}()
	COMMON.PDATA0.VMIN = func() *[]int {
		arr := make([]int, 3)
		return &arr
	}()
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
	PROMPT := new(bool)
	DEFALT := new(bool)
	PSCOLR := new(bool)
	PSREVS := new(bool)
	COMMON.PRMPTD.PSREVS = new(float64)
	COMMON.PRMPTD.PSCOLR = new(float64)
	COMMON.PRMPTD.DEFALT = new(float64)
	COMMON.PRMPTD.PROMPT = new(int)
	HDCPY := new(bool)
	HDLOGO := new(bool)
	PSFRAM := new(bool)
	COMMON.PDATPS.PSFRAM = new(float64)
	COMMON.PDATPS.HDLOGO = new(float64)
	COMMON.PDATPS.HDCPY = new(int)
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
	HDCPYO := new(bool)
	ERRCK := new(bool)
	PINPUT := new(bool)
	IUSED := new(int)
	IFRFL := new(int)
	I := new(int)
	J := new(int)
	KIND := func() *[]float64 {
		arr := make([]float64, -1)
		return &arr
	}()
	//!      * * F E A P * * A Finite Element Analysis Program

	//!....  Copyright (c) 1984-2021: Regents of the University of California

	//!                               All rights reserved

	//!-----[--.----+----.----+----.-----------------------------------------]

	//!      Purpose: Input of perspective parameters

	//!               kpers: flag for perspective projection (1=perspective)

	//!      Inputs:

	//!         flag      - Flag, if true and not default user to provide

	//!                           data

	//!      Outputs:

	//!         none      - Outputs through common blocks

	//!-----[--.----+----.----+----.-----------------------------------------]

	IOR = COMMON.IOFILE.IOR
	IOW = COMMON.IOFILE.IOW
	KEEPFL = COMMON.IOFILE.KEEPFL
	VMIN = COMMON.PDATA0.VMIN
	VMAX = COMMON.PDATA0.VMAX
	E = COMMON.PPERS.E
	EOLD = COMMON.PPERS.EOLD
	TG = COMMON.PPERS.TG
	VOLD = COMMON.PPERS.VOLD
	Q = COMMON.PPERS.Q
	XLBDA = COMMON.PPERS.XLBDA
	ENORM = COMMON.PPERS.ENORM
	KPERS = COMMON.PPERS.KPERS
	PROMPT = COMMON.PRMPTD.PROMPT
	DEFALT = COMMON.PRMPTD.DEFALT
	PSCOLR = COMMON.PRMPTD.PSCOLR
	PSREVS = COMMON.PRMPTD.PSREVS
	HDCPY = COMMON.PDATPS.HDCPY
	HDLOGO = COMMON.PDATPS.HDLOGO
	PSFRAM = COMMON.PDATPS.PSFRAM
	NDF = COMMON.SDATA.NDF
	NDM = COMMON.SDATA.NDM
	NEN1 = COMMON.SDATA.NEN1
	NST = COMMON.SDATA.NST
	NNEQ = COMMON.SDATA.NNEQ
	NDL = COMMON.SDATA.NDL
	//F4GO: NOT IMPLEMENTED :real ( KIND = 8 ) :: T ( 3 , 3 ) , TGOLD ( 3 ) , V ( 3 ) , VNORM
Label1:
	;
	if *(FLAG) {
		if *DEFALT {
			(*IUSED) = 0
		} else {
			if (*IOR) < 0 {
				// Unused by f4go :  if ( IOR .lt. 0 ) write ( * , 2000 )
				fmt.Println("WRITE SOMETHING")
			}
			(*ERRCK) = (*PINPUT(E, func() *int { y := 1; return &y }()))
			(*IUSED) = (*NINT(E(func() *int { y := 1; return &y }())))
		}
		if (*IUSED) == 0 {
			if EOLD(func() *int { y := 1; return &y }())+EOLD(func() *int { y := 2; return &y }())+EOLD(func() *int { y := 3; return &y }()) == 0.0e0 {
				EOLD(func() *int { y := 1; return &y }()) = 3.0e0 * VMAX(func() *int { y := 1; return &y }())
				EOLD(func() *int { y := 2; return &y }()) = 2.0e0 * VMAX(func() *int { y := 2; return &y }())
				if (*NDM) <= 2 {
					EOLD(func() *int { y := 3; return &y }()) = 1.5e0 * intrinsic.MAX(EOLD(func() *int { y := 1; return &y }()), EOLD(func() *int { y := 2; return &y }()))
				} else {
					EOLD(func() *int { y := 3; return &y }()) = 1.5e0 * VMAX(func() *int { y := 3; return &y }())
				}
				if (*EOLD(func() *int { y := 1; return &y }())) == 0.0e0 {
					EOLD(func() *int { y := 1; return &y }()) = 0.4e0 * intrinsic.MAX(EOLD(func() *int { y := 2; return &y }()), EOLD(func() *int { y := 3; return &y }()))
				}
				if (*EOLD(func() *int { y := 2; return &y }())) == 0.0e0 {
					EOLD(func() *int { y := 2; return &y }()) = 0.4e0 * intrinsic.MAX(EOLD(func() *int { y := 3; return &y }()), EOLD(func() *int { y := 1; return &y }()))
				}
				if (*EOLD(func() *int { y := 3; return &y }())) == 0.0e0 {
					if (*NDM) <= 2 {
						EOLD(func() *int { y := 3; return &y }()) = 1.4e0 * intrinsic.MAX(EOLD(func() *int { y := 1; return &y }()), EOLD(func() *int { y := 2; return &y }()))
					} else {
						EOLD(func() *int { y := 3; return &y }()) = 0.4e0 * intrinsic.MAX(EOLD(func() *int { y := 1; return &y }()), EOLD(func() *int { y := 2; return &y }()))
					}
				}
			}
			if *DEFALT {
				PZERO(E, func() *int { y := 3; return &y }())
			} else {
				if (*IOR) < 0 {
					// Unused by f4go :  if ( IOR .lt. 0 ) write ( * , 2001 ) VMIN , VMAX , EOLD
					fmt.Println("WRITE SOMETHING")
				}
				(*ERRCK) = (*PINPUT(E, func() *int { y := 3; return &y }()))
			}
			if E(func() *int { y := 1; return &y }())+E(func() *int { y := 2; return &y }())+E(func() *int { y := 3; return &y }()) == 0.0e0 {
				E(func() *int { y := 1; return &y }()) = (*EOLD(func() *int { y := 1; return &y }()))
				E(func() *int { y := 2; return &y }()) = (*EOLD(func() *int { y := 2; return &y }()))
				E(func() *int { y := 3; return &y }()) = (*EOLD(func() *int { y := 3; return &y }()))
			} else {
				EOLD(func() *int { y := 1; return &y }()) = (*E(func() *int { y := 1; return &y }()))
				EOLD(func() *int { y := 2; return &y }()) = (*E(func() *int { y := 2; return &y }()))
				EOLD(func() *int { y := 3; return &y }()) = (*E(func() *int { y := 3; return &y }()))
			}
			// Unused by f4go :  write ( IOW , 2002 ) VMIN , VMAX , E
			fmt.Println("WRITE SOMETHING")
			TG(func() *int { y := 1; return &y }()) = 0.5e0 * (VMAX(func() *int { y := 1; return &y }()) + VMIN(func() *int { y := 1; return &y }()))
			TG(func() *int { y := 2; return &y }()) = 0.5e0 * (VMAX(func() *int { y := 2; return &y }()) + VMIN(func() *int { y := 2; return &y }()))
			TG(func() *int { y := 3; return &y }()) = 0.5e0 * (VMAX(func() *int { y := 3; return &y }()) + VMIN(func() *int { y := 3; return &y }()))
			TGOLD(func() *int { y := 1; return &y }()) = (*TG(func() *int { y := 1; return &y }()))
			TGOLD(func() *int { y := 2; return &y }()) = (*TG(func() *int { y := 2; return &y }()))
			TGOLD(func() *int { y := 3; return &y }()) = (*TG(func() *int { y := 3; return &y }()))
			if *DEFALT {
				PZERO(V, func() *int { y := 3; return &y }())
			} else {
				if (*IOR) < 0 {
					// Unused by f4go :  if ( IOR .lt. 0 ) write ( * , 2003 ) VOLD
					fmt.Println("WRITE SOMETHING")
				}
				(*ERRCK) = (*PINPUT(V, func() *int { y := 3; return &y }()))
			}
			if V(func() *int { y := 1; return &y }())+V(func() *int { y := 2; return &y }())+V(func() *int { y := 3; return &y }()) == 0.0e0 {
				V(func() *int { y := 1; return &y }()) = (*VOLD(func() *int { y := 1; return &y }()))
				V(func() *int { y := 2; return &y }()) = (*VOLD(func() *int { y := 2; return &y }()))
				V(func() *int { y := 3; return &y }()) = (*VOLD(func() *int { y := 3; return &y }()))
			} else {
				VOLD(func() *int { y := 1; return &y }()) = (*V(func() *int { y := 1; return &y }()))
				VOLD(func() *int { y := 2; return &y }()) = (*V(func() *int { y := 2; return &y }()))
				VOLD(func() *int { y := 3; return &y }()) = (*V(func() *int { y := 3; return &y }()))
			}
			// Unused by f4go :  write ( IOW , 2004 ) V
			fmt.Println("WRITE SOMETHING")
		}
	}
	if !(*(FLAG)) || (*IUSED) != 0 {
		E(func() *int { y := 1; return &y }()) = (*EOLD(func() *int { y := 1; return &y }()))
		E(func() *int { y := 2; return &y }()) = (*EOLD(func() *int { y := 2; return &y }()))
		E(func() *int { y := 3; return &y }()) = (*EOLD(func() *int { y := 3; return &y }()))
		TG(func() *int { y := 1; return &y }()) = (*TGOLD(func() *int { y := 1; return &y }()))
		TG(func() *int { y := 2; return &y }()) = (*TGOLD(func() *int { y := 2; return &y }()))
		TG(func() *int { y := 3; return &y }()) = (*TGOLD(func() *int { y := 3; return &y }()))
		V(func() *int { y := 1; return &y }()) = (*VOLD(func() *int { y := 1; return &y }()))
		V(func() *int { y := 2; return &y }()) = (*VOLD(func() *int { y := 2; return &y }()))
		V(func() *int { y := 3; return &y }()) = (*VOLD(func() *int { y := 3; return &y }()))
	}
	//!     Projection matrix t

	Q(func() *int { y := 1; return &y }(), func() *int { y := 1; return &y }()) = E(func() *int { y := 1; return &y }()) - TG(func() *int { y := 1; return &y }())
	Q(func() *int { y := 2; return &y }(), func() *int { y := 1; return &y }()) = E(func() *int { y := 2; return &y }()) - TG(func() *int { y := 2; return &y }())
	Q(func() *int { y := 3; return &y }(), func() *int { y := 1; return &y }()) = E(func() *int { y := 3; return &y }()) - TG(func() *int { y := 3; return &y }())
	(*ENORM) = (intrinsic.SQRT(Q(func() *int { y := 1; return &y }(), func() *int { y := 1; return &y }())*Q(func() *int { y := 1; return &y }(), func() *int { y := 1; return &y }()) + Q(func() *int { y := 2; return &y }(), func() *int { y := 1; return &y }())*Q(func() *int { y := 2; return &y }(), func() *int { y := 1; return &y }()) + Q(func() *int { y := 3; return &y }(), func() *int { y := 1; return &y }())*Q(func() *int { y := 3; return &y }(), func() *int { y := 1; return &y }())))
	if (*ENORM) <= 0.0e0 {
		goto Label901
		//
	}
	//!     PostScript

	(*HDCPYO) = (*HDCPY)
	if (*HDLOGO) && (*HDCPY) {
		(*HDCPY) = false
		(*IFRFL) = 1
	}
	PLVIEW(Q, VMIN, VMAX)
	if (*IFRFL) == 1 {
		(*HDCPY) = (*HDCPYO)
		(*IFRFL) = 0
	}
	for (*I) = 1; (*I) <= 3; (*I)++ {
		Q(I, func() *int { y := 3; return &y }()) = Q(I, func() *int { y := 1; return &y }()) / (*ENORM)
	}
	for (*I) = 1; (*I) <= 3; (*I)++ {
		for (*J) = 1; (*J) <= 3; (*J)++ {
			T(I, J) = -Q(I, func() *int { y := 3; return &y }()) * Q(J, func() *int { y := 3; return &y }())
		}
		T(I, I) = T(I, I) + 1.e0
	}
	//!     Find projection of v

	for (*I) = 1; (*I) <= 3; (*I)++ {
		Q(I, func() *int { y := 2; return &y }()) = T(I, func() *int { y := 1; return &y }())*V(func() *int { y := 1; return &y }()) + T(I, func() *int { y := 2; return &y }())*V(func() *int { y := 2; return &y }()) + T(I, func() *int { y := 3; return &y }())*V(func() *int { y := 3; return &y }())
	}
	(*VNORM) = (intrinsic.SQRT(Q(func() *int { y := 1; return &y }(), func() *int { y := 2; return &y }())*Q(func() *int { y := 1; return &y }(), func() *int { y := 2; return &y }()) + Q(func() *int { y := 2; return &y }(), func() *int { y := 2; return &y }())*Q(func() *int { y := 2; return &y }(), func() *int { y := 2; return &y }()) + Q(func() *int { y := 3; return &y }(), func() *int { y := 2; return &y }())*Q(func() *int { y := 3; return &y }(), func() *int { y := 2; return &y }())))
	if (*VNORM) <= 0.0e0 {
		goto Label901
		//  do I = 1 , 3
	}
	for (*I) = 1; (*I) <= 3; (*I)++ {
		Q(I, func() *int { y := 2; return &y }()) = Q(I, func() *int { y := 2; return &y }()) / (*VNORM)
	}
	//!     Compute normal vector

	VECP(Q(func() *int { y := 1; return &y }(), func() *int { y := 2; return &y }()), Q(func() *int { y := 1; return &y }(), func() *int { y := 3; return &y }()), Q(func() *int { y := 1; return &y }(), func() *int { y := 1; return &y }()))
	//!     Compute rotation matrix

	for (*I) = 1; (*I) <= 3; (*I)++ {
		for (*J) = 1; (*J) <= 3; (*J)++ {
			XLBDA(I, J) = Q(func() *int { y := 1; return &y }(), I)*T(func() *int { y := 1; return &y }(), J) + Q(func() *int { y := 2; return &y }(), I)*T(func() *int { y := 2; return &y }(), J) + Q(func() *int { y := 3; return &y }(), I)*T(func() *int { y := 3; return &y }(), J)
		}
	}
	XLBDA(func() *int { y := 3; return &y }(), func() *int { y := 1; return &y }()) = XLBDA(func() *int { y := 1; return &y }(), func() *int { y := 2; return &y }())*XLBDA(func() *int { y := 2; return &y }(), func() *int { y := 3; return &y }()) - XLBDA(func() *int { y := 1; return &y }(), func() *int { y := 3; return &y }())*XLBDA(func() *int { y := 2; return &y }(), func() *int { y := 2; return &y }())
	XLBDA(func() *int { y := 3; return &y }(), func() *int { y := 2; return &y }()) = XLBDA(func() *int { y := 1; return &y }(), func() *int { y := 3; return &y }())*XLBDA(func() *int { y := 2; return &y }(), func() *int { y := 1; return &y }()) - XLBDA(func() *int { y := 1; return &y }(), func() *int { y := 1; return &y }())*XLBDA(func() *int { y := 2; return &y }(), func() *int { y := 3; return &y }())
	XLBDA(func() *int { y := 3; return &y }(), func() *int { y := 3; return &y }()) = XLBDA(func() *int { y := 1; return &y }(), func() *int { y := 1; return &y }())*XLBDA(func() *int { y := 2; return &y }(), func() *int { y := 2; return &y }()) - XLBDA(func() *int { y := 1; return &y }(), func() *int { y := 2; return &y }())*XLBDA(func() *int { y := 2; return &y }(), func() *int { y := 1; return &y }())
	return
Label901:
	;
	if (*IOR) < 0 {
		// Unused by f4go :  901 if ( IOR .lt. 0 ) write ( * , 3000 )
		fmt.Println("WRITE SOMETHING")
	}
	if (*IOR) > 0 {
		// Unused by f4go :  if ( IOR .gt. 0 ) write ( IOW , 3000 )
		fmt.Println("WRITE SOMETHING")
	}
	ERRCLR(func() *[]byte { y := []byte("PERSPE"); return &y }())
	if *(FLAG) {
		goto Label1
		//
	}
	//!     Formats

	//Label2000:

	// Unused by f4go :  2000 format ( " Use old parameters? (0 = new or 1 = old) :" , $ )
	//Label2001:

	// Unused by f4go :  2001 format ( " Body occupies the space with:" / "                 X" , 13 X , "Y" , 13 X , "Z" / "   Minimum " , 1 P3E14 . 5 / "   Maximum " , 1 P3E14 . 5 / " Enter coordinates of view point   (X,Y,Z)." / " Default: X=" , 1 P , 1e9 .2 , ", Y=" , 1 P , 1e9 .2 , ", Z=" , 1 P , 1e9 .2 / "  >" , $ )
	//Label2002:

	// Unused by f4go :  2002 format ( " Body occupies the space with:" / "                 X" , 13 X , "Y" , 13 X , "Z" / "   Minimum " , 1 P3E14 . 5 / "   Maximum " , 1 P3E14 . 5 / "          X=" , 1 P , 1e9 .2 , ", Y=" , 1 P , 1e9 .2 , ", Z=" , 1 P , 1e9 .2 / )
	//Label2003:

	// Unused by f4go :  2003 format ( " Enter comps. of vertical vector   (X,Y,Z)." / " Default: X=" , 1 P , 1e9 .2 , ", Y=" , 1 P , 1e9 .2 , ", Z=" , 1 P , 1e9 .2 / "  >" , $ )
	//Label2004:

	// Unused by f4go :  2004 format ( "   Components of vertical vector   (X,Y,Z)." / "          X=" , 1 P , 1e9 .2 , ", Y=" , 1 P , 1e9 .2 , ", Z=" , 1 P , 1e9 .2 / )
	//Label3000:

	// Unused by f4go :  3000 format ( " *ERROR* Improper view specified" )
}
