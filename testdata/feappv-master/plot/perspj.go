package main

type MEMORY struct {
	IOFILE struct {
		KEEPFL *bool
		IOR    *int
		IOW    *int
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
	PVIEW struct {
		CS    *float64
		ZVIEW *float64
		IVIEW *int
		LVIEW *bool
	}
}

var COMMON MEMORY

// !$Id:$
func PERSPJ(XP *int, NTYP *int, NUMNP *int, ERRV *bool) {
	IOR := new(int)
	IOW := new(int)
	COMMON.IOFILE.IOW = new(float64)
	COMMON.IOFILE.IOR = new(int)
	KEEPFL := new(bool)
	COMMON.IOFILE.KEEPFL = new(int)
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
	CS := new(float64)
	ZVIEW := new(float64)
	IVIEW := new(int)
	LVIEW := new(bool)
	COMMON.PVIEW.LVIEW = new(float64)
	COMMON.PVIEW.IVIEW = new(float64)
	COMMON.PVIEW.ZVIEW = new(float64)
	COMMON.PVIEW.CS = new(int)
	I := new(int)
	N := new(int)
	KIND := func() *[]float64 {
		arr := make([]float64, -1)
		return &arr
	}()
	//!      * * F E A P * * A Finite Element Analysis Program

	//!....  Copyright (c) 1984-2021: Regents of the University of California

	//!                               All rights reserved

	//!-----[--.----+----.----+----.-----------------------------------------]

	//!      Purpose: Compute perspective projection of coordinates

	//!      Inputs:

	//!         xp(3,*) - Global coordinates

	//!         ntyp(*) - Active node indicator

	//!         numnp   - Total number of nodal points

	//!      Outputs:

	//!         xp(3,*) -Perspective projected coordinates

	//!-----[--.----+----.----+----.-----------------------------------------]

	IOR = COMMON.IOFILE.IOR
	IOW = COMMON.IOFILE.IOW
	KEEPFL = COMMON.IOFILE.KEEPFL
	E = COMMON.PPERS.E
	EOLD = COMMON.PPERS.EOLD
	TG = COMMON.PPERS.TG
	VOLD = COMMON.PPERS.VOLD
	Q = COMMON.PPERS.Q
	XLBDA = COMMON.PPERS.XLBDA
	ENORM = COMMON.PPERS.ENORM
	KPERS = COMMON.PPERS.KPERS
	CS = COMMON.PVIEW.CS
	ZVIEW = COMMON.PVIEW.ZVIEW
	IVIEW = COMMON.PVIEW.IVIEW
	LVIEW = COMMON.PVIEW.LVIEW
	//F4GO: NOT IMPLEMENTED :real ( KIND = 8 ) :: T1 ( 3 ) , XP ( 3 , NUMNP ) , ALPHA
	//!     Loop over data points and find projection

	for (*N) = 1; (*N) <= (*(NUMNP)); (*N)++ {
		if (*(NTYP)(N)) >= 0 {
			(*LVIEW) = false
			for (*I) = 1; (*I) <= 3; (*I)++ {
				T1(I) = XP(I, N) - E(I)
			}
			for (*I) = 1; (*I) <= 3; (*I)++ {
				XP(I, N) = XLBDA(I, func() *int { y := 1; return &y }())*T1(func() *int { y := 1; return &y }()) + XLBDA(I, func() *int { y := 2; return &y }())*T1(func() *int { y := 2; return &y }()) + XLBDA(I, func() *int { y := 3; return &y }())*T1(func() *int { y := 3; return &y }())
			}
			(*ALPHA) = -(*ENORM) / (T1(func() *int { y := 1; return &y }())*Q(func() *int { y := 1; return &y }(), func() *int { y := 3; return &y }()) + T1(func() *int { y := 2; return &y }())*Q(func() *int { y := 2; return &y }(), func() *int { y := 3; return &y }()) + T1(func() *int { y := 3; return &y }())*Q(func() *int { y := 3; return &y }(), func() *int { y := 3; return &y }()))
			if (*ALPHA) < 0.e0 {
				(*(ERRV)) = true
				if (*IOR) < 0 {
					// Unused by f4go :  write ( * , 2000 )
					fmt.Println("WRITE SOMETHING")
					return
				} else {
					// Unused by f4go :  write ( IOW , 2000 )
					fmt.Println("WRITE SOMETHING")
					PLSTOP(&(true))
				}
			} else if (*XP(func() *int { y := 3; return &y }(), N)) > (*ZVIEW) {
				(*LVIEW) = true
				(*(ERRV)) = false
			} else {
				(*(ERRV)) = false
			}
			for (*I) = 1; (*I) <= 3; (*I)++ {
				XP(I, N) = (*ALPHA) * XP(I, N)
			}
		}
	}
	//Label2000:

	// Unused by f4go :  2000 format ( // 1 X , " Point too close, choose another one!" // )
}
