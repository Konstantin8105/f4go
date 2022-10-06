package main

import "github.com/Konstantin8105/f4go/intrinsic"

type MEMORY struct {
	EQSYM struct {
		NEQS *int
	}
	PDATA1 struct {
		SCALEF *float64
		SCALEG *float64
		S0     *float64
		DX     *float64
		SX     *float64
		FACT   *float64
	}
}

var COMMON MEMORY

// !$Id:$
func PLTPRF(JP *int, NEQ *int, LOWER *bool) {
	NEQS := new(int)
	COMMON.EQSYM.NEQS = new(int)
	SCALEF := new(float64)
	SCALEG := new(float64)
	S0 := new(float64)
	DX := new(float64)
	SX := new(float64)
	FACT := new(float64)
	COMMON.PDATA1.FACT = new(float64)
	COMMON.PDATA1.SX = func() *[]float64 {
		arr := make([]float64, 2)
		return &arr
	}()
	COMMON.PDATA1.DX(2) = new(float64)
	COMMON.PDATA1.S0 = func() *[]float64 {
		arr := make([]float64, 2)
		return &arr
	}()
	COMMON.PDATA1.SCALEG = new(float64)
	COMMON.PDATA1.SCALEF = new(int)
	N := new(int)
	KIND := func() *[]float64 {
		arr := make([]float64, -1)
		return &arr
	}()
	//!      * * F E A P * * A Finite Element Analysis Program

	//!....  Copyright (c) 1984-2021: Regents of the University of California

	//!                               All rights reserved

	//!-----[--.----+----.----+----.-----------------------------------------]

	//!     Purpose: Display plot of profile layout

	//!     Inputs:

	//!        jp(*)   - Column pointers

	//!        neq     - Number of equations

	//!     Outputs:

	//!        none

	//!-----[--.----+----.----+----.-----------------------------------------]

	NEQS = COMMON.EQSYM.NEQS
	SCALEF = COMMON.PDATA1.SCALEF
	SCALEG = COMMON.PDATA1.SCALEG
	S0 = COMMON.PDATA1.S0
	DX = COMMON.PDATA1.DX
	SX = COMMON.PDATA1.SX
	FACT = COMMON.PDATA1.FACT
	//F4GO: NOT IMPLEMENTED :real ( KIND = 8 ) :: X0 , Y0 , X , Y , C , PFACT , RFACT , PTONE , PTNIN , ONEPT
	//!     Set plot factors and start coordinate

	(*RFACT) = 0.5e0 / ((*SCALEG) * (*FACT))
	(*PFACT) = 0.8e0 / intrinsic.DBLE((*(NEQ))) * (*RFACT)
	(*X0) = 0.5e0*SX(func() *int { y := 1; return &y }()) - 0.5e0*(*RFACT)
	(*Y0) = 0.5e0*SX(func() *int { y := 2; return &y }()) - 0.5e0*(*RFACT)
	(*PTONE) = 0.1e0 * (*RFACT)
	(*ONEPT) = 1.0e0 * (*RFACT)
	(*PTNIN) = 0.9e0 * (*RFACT)
	//!     Upper part

	PPPCOL(func() *int { y := 3; return &y }(), func() *int { y := 1; return &y }())
	for (*N) = 2; (*N) <= (*(NEQ)); (*N)++ {
		(*X) = (*PTONE) + intrinsic.DBLE((*N))*(*PFACT)
		(*Y) = (*ONEPT) - (*X) + (*Y0)
		(*C) = intrinsic.DBLE((JP)(N)-(JP)((*N)-1)) * (*PFACT)
		(*X) = (*X) + (*X0)
		PLOTL(X, Y, func() *float64 { y := 0.0e0; return &y }(), func() *int { y := 3; return &y }())
		PLOTL(X, (*Y)+(*C), func() *float64 { y := 0.0e0; return &y }(), func() *int { y := 2; return &y }())
	}
	//! n

	//!     Lower part

	if *(LOWER) {
		PPPCOL(func() *int { y := 4; return &y }(), func() *int { y := 1; return &y }())
		for (*N) = 2; (*N) <= (*(NEQ)); (*N)++ {
			(*X) = (*PTONE) + intrinsic.DBLE((*N))*(*PFACT)
			(*Y) = (*ONEPT) - (*X) + (*Y0)
			(*C) = intrinsic.DBLE((JP)(N)-(JP)((*N)-1)) * (*PFACT)
			(*X) = (*X) + (*X0)
			PLOTL((*X)-(*C), Y, func() *float64 { y := 0.0e0; return &y }(), func() *int { y := 3; return &y }())
			PLOTL(X, Y, func() *float64 { y := 0.0e0; return &y }(), func() *int { y := 2; return &y }())
		}
		//! n

	}
	//!     Diagonal

	PPPCOL(func() *int { y := 2; return &y }(), func() *int { y := 1; return &y }())
	PLOTL((*PTONE)+(*PFACT)+(*X0), (*PTNIN)-(*PFACT)+(*Y0), func() *float64 { y := 0.0e0; return &y }(), func() *int { y := 3; return &y }())
	PLOTL((*PTNIN)+(*X0), (*PTONE)+(*Y0), func() *float64 { y := 0.0e0; return &y }(), func() *int { y := 2; return &y }())
}
