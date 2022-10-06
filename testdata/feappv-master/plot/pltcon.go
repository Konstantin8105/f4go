package main

import "github.com/Konstantin8105/f4go/intrinsic"

type MEMORY struct {
	IOFILE struct {
		KEEPFL *bool
		IOR    *int
		IOW    *int
	}
	PDATA4 struct {
		XMIN *float64
		XMAX *float64
		NZM1 *int
		NZM2 *int
		NFAC *int
		NFMX *int
		PDF  *int
	}
	CDATA struct {
		NUMNP  *int
		NUMEL  *int
		NUMMAT *int
		NEN    *int
		NEQ    *int
		IPR    *int
		NETYP  *int
	}
	COMREC struct {
		RECORD *[   ]byte
	}
	COMINT struct {
		FINDEX *int
	}
	COMFIL struct {
		FINP   *[   ]byte
		FOUT   *[   ]byte
		FRES   *[   ]byte
		FSAV   *[   ]byte
		FPLT   *[   ]byte
		FINCLD *[   ]byte
	}
	PBODY struct {
		MAPLT *int
	}
	PDATRI struct {
		XMX *float64
		XMN *float64
		YMX *float64
		YMN *float64
	}
	PRMPTD struct {
		PROMPT *bool
		DEFALT *bool
		PSCOLR *bool
		PSREVS *bool
	}
	PSDAT1 struct {
		PSMX *float64
		PSMN *float64
		XPSX *float64
		XPSN *float64
	}
	PLCON1 struct {
		VLU   *float64
		DX1   *float64
		NLABI *int
		VFLG  *bool
		TVC   *bool
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
	PRANGE struct {
		RANGMN *float64
		RANGMX *float64
		RANGFL *bool
	}
	PDATA6 struct {
		INORD *int
		IPORD *int
		IPU   *int
	}
	RPDATA struct {
		RMN *float64
		RMX *float64
	}
	FDATA struct {
		FL  *bool
		PFR *bool
	}
	PDATA1 struct {
		SCALEF *float64
		SCALEG *float64
		S0     *float64
		DX     *float64
		SX     *float64
		FACT   *float64
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
//!$Id:$
func PLTCON(X *int, IE *int, IX *int, IP *int, U *int, NIE *int, NDM *int, NDF *int, NEN1 *int, NEN0 *int, IC *int, MC *int, LC *int, MMC *int, LABEL *bool) {
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
	FINP := func() *[]byte {
		arr := make([]byte, 128)
		return &arr
	}()
	FOUT := func() *[]byte {
		arr := make([]byte, 128)
		return &arr
	}()
	FRES := func() *[]byte {
		arr := make([]byte, 128)
		return &arr
	}()
	FSAV := func() *[]byte {
		arr := make([]byte, 128)
		return &arr
	}()
	FPLT := func() *[]byte {
		arr := make([]byte, 128)
		return &arr
	}()
	FINCLD := func() *[]byte {
		arr := make([]byte, 128)
		return &arr
	}()
	COMMON.COMFIL.FINCLD = func() *[]float64 {
		arr := make([]float64, 10)
		return &arr
	}()
	COMMON.COMFIL.FPLT = new(float64)
	COMMON.COMFIL.FSAV = new(float64)
	COMMON.COMFIL.FRES = new(float64)
	COMMON.COMFIL.FOUT = new(float64)
	COMMON.COMFIL.FINP = new(int)
	RECORD := func() *[]byte {
		arr := make([]byte, 256)
		return &arr
	}()
	COMMON.COMREC.RECORD = new(int)
	FINDEX := new(int)
	COMMON.COMINT.FINDEX = new(int)
	FL := new(bool)
	PFR := new(bool)
	COMMON.FDATA.PFR = new(float64)
	COMMON.FDATA.FL = func() *[]int {
		arr := make([]int, 12)
		return &arr
	}()
	IOR := new(int)
	IOW := new(int)
	COMMON.IOFILE.IOW = new(float64)
	COMMON.IOFILE.IOR = new(int)
	KEEPFL := new(bool)
	COMMON.IOFILE.KEEPFL = new(int)
	MAPLT := new(int)
	COMMON.PBODY.MAPLT = new(int)
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
	XMIN := new(float64)
	XMAX := new(float64)
	NZM1 := new(int)
	NZM2 := new(int)
	NFAC := new(int)
	NFMX := new(int)
	PDF := new(int)
	COMMON.PDATA4.PDF = func() *[]float64 {
		arr := make([]float64, 3)
		return &arr
	}()
	COMMON.PDATA4.NFMX = new(float64)
	COMMON.PDATA4.NFAC = new(float64)
	COMMON.PDATA4.NZM2 = new(float64)
	COMMON.PDATA4.NZM1 = new(float64)
	COMMON.PDATA4.XMAX = func() *[]float64 {
		arr := make([]float64, 3)
		return &arr
	}()
	COMMON.PDATA4.XMIN = func() *[]int {
		arr := make([]int, 3)
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
	RANGMN := new(float64)
	RANGMX := new(float64)
	RANGFL := new(bool)
	COMMON.PRANGE.RANGFL = new(float64)
	COMMON.PRANGE.RANGMX = new(float64)
	COMMON.PRANGE.RANGMN = new(int)
	PROMPT := new(bool)
	DEFALT := new(bool)
	PSCOLR := new(bool)
	PSREVS := new(bool)
	COMMON.PRMPTD.PSREVS = new(float64)
	COMMON.PRMPTD.PSCOLR = new(float64)
	COMMON.PRMPTD.DEFALT = new(float64)
	COMMON.PRMPTD.PROMPT = new(int)
	PSMX := new(float64)
	PSMN := new(float64)
	XPSX := new(float64)
	XPSN := new(float64)
	COMMON.PSDAT1.XPSN = func() *[]float64 {
		arr := make([]float64, 3)
		return &arr
	}()
	COMMON.PSDAT1.XPSX = func() *[]float64 {
		arr := make([]float64, 3)
		return &arr
	}()
	COMMON.PSDAT1.PSMN = new(float64)
	COMMON.PSDAT1.PSMX = new(int)
	RMN := new(float64)
	RMX := new(float64)
	COMMON.RPDATA.RMX = new(float64)
	COMMON.RPDATA.RMN = new(int)
	LEN := func() *[]byte {
		arr := make([]byte, -1)
		return &arr
	}()
	ERRCK := new(bool)
	CONT := new(bool)
	PINPUT := new(bool)
	LABL := new(bool)
	CINPUT := new(bool)
	I := new(int)
	J := new(int)
	N := new(int)
	MA := new(int)
	NC := new(int)
	NNC := new(int)
	NERR := new(int)
	NUMF := new(int)
	NUMFAC := new(int)
	IEL := new(int)
	IUF := new(int)
	IUTOT := new(int)
	NS := new(int)
	II := new(int)
	KO := new(int)
	NE := new(int)
	NEL := new(int)
	PSTYP := new(int)
	KIND := func() *[]float64 {
		arr := make([]float64, -1)
		return &arr
	}()
	IPLT := new(int)
	ICL := new(int)
	ILQ := new(int)
	IQ9 := new(int)
	IQ16 := new(int)
	IT6 := new(int)
	KIND := func() *[]float64 {
		arr := make([]float64, -1)
		return &arr
	}()
	//!      * * F E A P * * A Finite Element Analysis Program

	//!....  Copyright (c) 1984-2021: Regents of the University of California

	//!                               All rights reserved

	//!-----[--.----+----.----+----.-----------------------------------------]

	//!      Purpose: Plot of mesh contours: With inter-element smoothing

	//!      Inputs:

	//!         x(ndm,*)  - Nodal coordinates of mesh

	//!         ie(nie,*) - Assembly data for material sets

	//!         ix(nen1,*)- Element nodal connections

	//!         ip(*)     - Sorted element order

	//!         u(*)      - Solution state

	//!         nie       - Dimension of ie array

	//!         ndm       - Dimension of x array

	//!         ndf       - Number dof/node

	//!         nen1      - Dimension of ix array

	//!         nen0      - Number nodes on plot face

	//!         ic        - Component number to plot

	//!         mc        - Number of contour lines: < 0 fill; > 0 line

	//!         lc        - Dimensioning information on component to plot

	//!         mmc       - Type of plot

	//!         label     - Flag, put labels on plots if true

	//!      Outputs:

	//!         none      - Plot outputs to screen/file

	//!-----[--.----+----.----+----.-----------------------------------------]

	NUMNP = COMMON.CDATA.NUMNP
	NUMEL = COMMON.CDATA.NUMEL
	NUMMAT = COMMON.CDATA.NUMMAT
	NEN = COMMON.CDATA.NEN
	NEQ = COMMON.CDATA.NEQ
	IPR = COMMON.CDATA.IPR
	NETYP = COMMON.CDATA.NETYP
	FINP = COMMON.COMFIL.FINP
	FOUT = COMMON.COMFIL.FOUT
	FRES = COMMON.COMFIL.FRES
	FSAV = COMMON.COMFIL.FSAV
	FPLT = COMMON.COMFIL.FPLT
	FINCLD = COMMON.COMFIL.FINCLD
	RECORD = COMMON.COMREC.RECORD
	FINDEX = COMMON.COMINT.FINDEX
	FL = COMMON.FDATA.FL
	PFR = COMMON.FDATA.PFR
	IOR = COMMON.IOFILE.IOR
	IOW = COMMON.IOFILE.IOW
	KEEPFL = COMMON.IOFILE.KEEPFL
	MAPLT = COMMON.PBODY.MAPLT
	SCALEF = COMMON.PDATA1.SCALEF
	SCALEG = COMMON.PDATA1.SCALEG
	S0 = COMMON.PDATA1.S0
	DX = COMMON.PDATA1.DX
	SX = COMMON.PDATA1.SX
	FACT = COMMON.PDATA1.FACT
	ICLEAR = COMMON.PDATA2.ICLEAR
	IDEV = COMMON.PDATA2.IDEV
	IDX = COMMON.PDATA2.IDX
	IDY = COMMON.PDATA2.IDY
	IPB = COMMON.PDATA2.IPB
	IFRM = COMMON.PDATA2.IFRM
	ICOLR = COMMON.PDATA2.ICOLR
	ILNO = COMMON.PDATA2.ILNO
	XMIN = COMMON.PDATA4.XMIN
	XMAX = COMMON.PDATA4.XMAX
	NZM1 = COMMON.PDATA4.NZM1
	NZM2 = COMMON.PDATA4.NZM2
	NFAC = COMMON.PDATA4.NFAC
	NFMX = COMMON.PDATA4.NFMX
	PDF = COMMON.PDATA4.PDF
	INORD = COMMON.PDATA6.INORD
	IPORD = COMMON.PDATA6.IPORD
	IPU = COMMON.PDATA6.IPU
	XMX = COMMON.PDATRI.XMX
	XMN = COMMON.PDATRI.XMN
	YMX = COMMON.PDATRI.YMX
	YMN = COMMON.PDATRI.YMN
	VLU = COMMON.PLCON1.VLU
	DX1 = COMMON.PLCON1.DX1
	NLABI = COMMON.PLCON1.NLABI
	VFLG = COMMON.PLCON1.VFLG
	TVC = COMMON.PLCON1.TVC
	E = COMMON.PPERS.E
	EOLD = COMMON.PPERS.EOLD
	TG = COMMON.PPERS.TG
	VOLD = COMMON.PPERS.VOLD
	Q = COMMON.PPERS.Q
	XLBDA = COMMON.PPERS.XLBDA
	ENORM = COMMON.PPERS.ENORM
	KPERS = COMMON.PPERS.KPERS
	RANGMN = COMMON.PRANGE.RANGMN
	RANGMX = COMMON.PRANGE.RANGMX
	RANGFL = COMMON.PRANGE.RANGFL
	PROMPT = COMMON.PRMPTD.PROMPT
	DEFALT = COMMON.PRMPTD.DEFALT
	PSCOLR = COMMON.PRMPTD.PSCOLR
	PSREVS = COMMON.PRMPTD.PSREVS
	PSMX = COMMON.PSDAT1.PSMX
	PSMN = COMMON.PSDAT1.PSMN
	XPSX = COMMON.PSDAT1.XPSX
	XPSN = COMMON.PSDAT1.XPSN
	RMN = COMMON.RPDATA.RMN
	RMX = COMMON.RPDATA.RMX
	//F4GO: NOT IMPLEMENTED :real ( KIND = 8 ) :: VMX , VMN
	//F4GO: NOT IMPLEMENTED :real ( KIND = 8 ) :: XL ( 3 , 29 ) , XQ ( 3 , 4 ) , X ( NDM , * ) , U ( * ) , V ( 29 ) , VC ( 12 ) , VQ ( 4 )
	//F4GO: NOT IMPLEMENTED :data IT6 / 1 , 4 , 6 , 4 , 2 , 5 , 4 , 5 , 6 , 6 , 5 , 3 /
	//F4GO: NOT IMPLEMENTED :data IQ9 / 1 , 5 , 9 , 8 , 5 , 2 , 6 , 9 , 8 , 9 , 7 , 4 , 9 , 6 , 3 , 7 /
	//F4GO: NOT IMPLEMENTED :data IQ16 / 1 , 5 , 13 , 12 , 5 , 6 , 14 , 13 , 6 , 2 , 7 , 14 , 12 , 13 , 16 , 11 , 13 , 14 , 15 , 16 , 14 , 7 , 8 , 15 , 11 , 16 , 10 , 4 , 16 , 15 , 9 , 10 , 15 , 8 , 3 , 9 /
	//!     Contour plot routine for elements: lines if mc > 0;

	//!                                        fills if mc < 0

	(*CONT) = true
	(*LABL) = (*(LABEL))
	PZEROL(TVC, &(true), func()*int{y:=81;return &y}())
Label1:
	;
	if (*(MC)) > 0 {
		(*NC) = (intrinsic.MAX(1, intrinsic.MIN((*(MC)), int(12))))
		(*NLABI) = 0
		(*DX1) = .024e0 / (*SCALEF)
		(*VFLG) = (*IPB) == 0
		(*NERR) = 0
	Label11:
		;
		if (*IOR) < 0 {
			// Unused by f4go :  11 if ( IOR .lt. 0 ) write ( * , 2001 ) NC
			fmt.Println("WRITE SOMETHING")
		}
		(*NNC) = (intrinsic.MIN(int(8), (*NC)))
		if (*PROMPT) && !(*DEFALT) {
			if (*IOR) < 0 {
				// Unused by f4go :  if ( IOR .lt. 0 ) write ( * , 2009 )
				fmt.Println("WRITE SOMETHING")
			}
			(*ERRCK) = (*PINPUT(VC, NNC))
			(*NERR) = (*NERR) + 1
			if (*NERR) > 5 {
				if (*IOR) < 0 {
					return
				}
				PLSTOP(&(true))
			}
			if *ERRCK {
				goto Label11
				//  else
			}
		} else {
			VC(func()*int{y:=1;return &y}()) = 0.0e0
			VC(func()*int{y:=2;return &y}()) = 0.0e0
		}
		if (*NC) > 1 && VC(func()*int{y:=1;return &y}()) == 0.0e0 && VC(func()*int{y:=2;return &y}()) == 0.0e0 {
			VC(func()*int{y:=1;return &y}()) = (*RMN) + ((*RMX)-(*RMN))/((*NC)+1)
			VC(NC) = (*RMN) + (*NC)*((*RMX)-(*RMN))/((*NC)+1)
			for (*I) = 2; (*I) <= (*NC)-1; (*I)++ {
				VC(I) = VC(func()*int{y:=1;return &y}()) + (VC(NC)-VC(func()*int{y:=1;return &y}()))*((*I)-1)/((*NC)-1)
			}
		} else {
			if (*NC) > 8 {
				(*NNC) = intrinsic.MIN(int(12), (*NC)) - 8
				if (*IOR) < 0 {
					// Unused by f4go :  if ( IOR .lt. 0 ) write ( * , 2009 )
					fmt.Println("WRITE SOMETHING")
				}
				(*ERRCK) = (*PINPUT(VC(func()*int{y:=9;return &y}()), NNC))
				if *ERRCK {
					goto Label11
					//  endif
				}
			}
		}
		if *PROMPT {
			if *PFR {
				// Unused by f4go :  if ( PFR ) write ( IOW , 2000 ) ( VC ( I ) , I = 1 , NC )
				fmt.Println("WRITE SOMETHING")
			}
			if (*IOR) < 0 && !(*DEFALT) {
				// Unused by f4go :  write ( * , 2000 ) ( VC ( I ) , I = 1 , NC )
				fmt.Println("WRITE SOMETHING")
			}
		}
		//!       Input label and color for first contour

	Label13:
		;
		if (*PROMPT) && !(*DEFALT) {
			if (*IOR) < 0 {
				// Unused by f4go :  if ( IOR .lt. 0 ) write ( * , 2002 )
				fmt.Println("WRITE SOMETHING")
			}
			(*ERRCK) = (*PINPUT(VLU(func()*int{y:=1;return &y}()), func()*int{y:=1;return &y}()))
			if *ERRCK {
				goto Label13
				//  NLABI = MAX ( 1 , MIN ( INT ( VLU ( 1 ) ) , 12 ) ) - 1
			}
			(*NLABI) = intrinsic.MAX(1, intrinsic.MIN(INT(VLU(func()*int{y:=1;return &y}())), int(12))) - 1
			if (*NLABI)+(*NC) > 12 {
				if (*IOR) < 0 {
					// Unused by f4go :  if ( IOR .lt. 0 ) write ( * , 2007 )
					fmt.Println("WRITE SOMETHING")
				}
				(*NLABI) = 12 - (*NC)
			}
		} else {
			(*NLABI) = 0
		}
		//!     Inputs for filled plots

	} else {
		(*CONT) = false
		(*NC) = 6
		if (*IPB) >= 0 {
		Label15:
			;
			if *RANGFL {
				VC(func()*int{y:=1;return &y}()) = (*RANGMN)
				VC(func()*int{y:=2;return &y}()) = (*RANGMX)
			} else if (*PROMPT) && !(*DEFALT) {
				if (*IOR) < 0 {
					// Unused by f4go :  if ( IOR .lt. 0 ) write ( * , 2008 ) RMN , RMX
					fmt.Println("WRITE SOMETHING")
				}
				(*ERRCK) = (*PINPUT(VC, func()*int{y:=2;return &y}()))
				if *ERRCK {
					goto Label15
					//  else
				}
			} else {
				VC(func()*int{y:=1;return &y}()) = 0.0e0
				VC(func()*int{y:=2;return &y}()) = 0.0e0
			}
			if (*VC(func()*int{y:=1;return &y}())) == (*VC(func()*int{y:=2;return &y}())) {
				VC(func()*int{y:=1;return &y}()) = (*RMN) + ((*RMX)-(*RMN))/7.0e0
				VC(func()*int{y:=6;return &y}()) = (*RMN) + 6.e0*((*RMX)-(*RMN))/7.0e0
			} else {
				VC(func()*int{y:=6;return &y}()) = (*VC(func()*int{y:=2;return &y}()))
			}
			for (*I) = 2; (*I) <= 5; (*I)++ {
				VC(I) = VC(func()*int{y:=1;return &y}()) + (VC(func()*int{y:=6;return &y}())-VC(func()*int{y:=1;return &y}()))*((*I)-1)/5.0e0
			}
			if *PROMPT {
				if *PFR {
					// Unused by f4go :  if ( PFR ) write ( IOW , 2000 ) ( VC ( I ) , I = 1 , NC )
					fmt.Println("WRITE SOMETHING")
				}
				if (*IOR) < 0 && !(*DEFALT) {
					// Unused by f4go :  write ( * , 2000 ) ( VC ( I ) , I = 1 , NC )
					fmt.Println("WRITE SOMETHING")
				}
			}
		}
	}
	//!     If interactive, offer chance to change inputs

	if (*IOR) < 0 && (*PROMPT) && !(*DEFALT) {
		// Unused by f4go :  write ( * , 2006 )
		fmt.Println("WRITE SOMETHING")
		//!20      read (*,1000,err=21,end=22) y

	Label20:
		;
		if !CINPUT() {
			goto Label22
			//  end
		}
		(*Y) = (*RECORD)[1-(1)]
		goto Label23
		//
		//!21     call  errclr ("PLTCON")

		ERRCLR(func()*[]byte{y:=[]byte("PLTCON");return &y}())
		goto Label20
		//  22 call ENDCLR ( "PLTCON" , Y )
	Label22:
		;
		ENDCLR(func()*[]byte{y:=[]byte("PLTCON");return &y}(), Y)
	Label23:
		;
		if (*Y) == 'c' || (*Y) == 'C' {
			return
		}
		if (*Y) != 'y' && (*Y) != 'Y' {
			goto Label1
			//  endif
		}
	}
	//!     Find max/min of plot variable

	PLOPEN()
	(*J) = (*(IC))
	(*XMX) = (*X(func()*int{y:=1;return &y}(), func()*int{y:=1;return &y}()))
	(*YMX) = (*X(func()*int{y:=2;return &y}(), func()*int{y:=1;return &y}()))
	(*XMN) = (*X(func()*int{y:=1;return &y}(), func()*int{y:=1;return &y}()))
	(*YMN) = (*X(func()*int{y:=2;return &y}(), func()*int{y:=1;return &y}()))
	(*VMN) = (*U(J))
	(*VMX) = (*U(J))
	for (*I) = 1; (*I) <= (*NUMNP); (*I)++ {
		(*XMX) = (intrinsic.MAX(X(func()*int{y:=1;return &y}(), I), (*XMX)))
		(*YMX) = (intrinsic.MAX(X(func()*int{y:=2;return &y}(), I), (*YMX)))
		(*XMN) = (intrinsic.MIN(X(func()*int{y:=1;return &y}(), I), (*XMN)))
		(*YMN) = (intrinsic.MIN(X(func()*int{y:=2;return &y}(), I), (*YMN)))
		(*VMN) = (intrinsic.MIN((*VMN), U(J)))
		(*VMX) = (intrinsic.MAX((*VMX), U(J)))
		(*J) = (*J) + (*(NDF))
	}
	if (*XMX) != (*XMN) {
		(*XMX) = 8.2e0 / ((*XMX) - (*XMN))
	}
	if (*YMX) != (*YMN) {
		(*YMX) = 8.2e0 / ((*YMX) - (*YMN))
	}
	if (*VMN) == 00e0 && (*VMX) == 0.0e0 {
		// Unused by f4go :  write ( IOW , 2005 )
		fmt.Println("WRITE SOMETHING")
		if (*IOR) < 0 {
			// Unused by f4go :  if ( IOR .lt. 0 ) write ( * , 2005 )
			fmt.Println("WRITE SOMETHING")
		}
		return
	}
	//!     Open plot and loop through elements

	PZERO(XL, 3*intrinsic.MAX(4, (*NEN)))
	(*(IC)) = (intrinsic.MAX(1, intrinsic.MIN((*(IC)), (*(NDF)))))
	(*PSMX) = (*VMN) - 1.
	(*PSMN) = (*VMX) + 1.
	for (*NE) = 1; (*NE) <= (*NFAC); (*NE)++ {
		//!       Get plot order for each element

		(*N) = (*(IP)(NE))
		//!       Plot active regions: material number: maplt; all if maplt = 0;

		if (*N) > 0 {
			(*MA) = (*(IX)((NEN1), N))
			(*PSTYP) = (*(IE)(func()*int{y:=1;return &y}(), MA))
			if (*PSTYP) > 0 && (IX)((*(NEN1))-1, N) >= 0 && ((*MAPLT) == 0 || (*MA) == (*MAPLT)) {
				(*IEL) = (*(IE)((*(NIE))-1, MA))
				//! iel

				//!           Determine maximum number of nodes on element

				for (*I) = (*(NEN0)); (*I) <= 1; (*I) += -1 {
					if (*(IX)(I, N)) != 0 {
						(*NEL) = (*I)
						(*EXIT)
					}
				}
				//! i

				(*NEL) = (intrinsic.MIN((*NEL), (*NEN)))
				if (*NEL) > 2 && (IX)(func()*int{y:=1;return &y}(), N) == (IX)(NEL, N) {
					(*NEL) = (*NEL) - 1
				}
				//!           Get plot order for each element

				if (*(IX)((*NEN)+7, N)) == -22 {
					(*PSTYP) = 2
				} else {
					PLFTYP(PSTYP, NEL, IEL)
				}
				//!           Perspective or standard feap elements

				if (*KPERS) == 1 || (*PSTYP) > 0 {
					//!             VEM 2-d elements

					if (*(IX)((*NEN)+7, N)) == -22 {
						VEM_COMPP((IX)((*NEN)+8, N), IPLT, NEL, IUF)
						//!             Standard FEAP elements

					} else {
						PLTORD((IX)(func()*int{y:=1;return &y}(), N), IEL, IUF, IPLT)
					}
					//!             Set values of vlu(1) and vlu(2)

					(*IUTOT) = 0
					VLU(func()*int{y:=1;return &y}()) = (*VMX)
					VLU(func()*int{y:=2;return &y}()) = (*VMN)
					for (*I) = 1; (*I) <= (*IUF); (*I)++ {
						(*NS) = (*IPLT(I))
						if (*NS) <= (*NEL) {
							(*II) = (*(IX)(IPLT(I), N))
							if (*II) > 0 {
								(*IUTOT) = (*IUTOT) + 1
								XL(func()*int{y:=1;return &y}(), NS) = (*X(func()*int{y:=1;return &y}(), II))
								XL(func()*int{y:=2;return &y}(), NS) = (*X(func()*int{y:=2;return &y}(), II))
								if (*(NDM)) >= 3 {
									XL(func()*int{y:=3;return &y}(), NS) = (*X(func()*int{y:=3;return &y}(), II))
								}
								(*J) = (*(NDF))*((*II)-1) + (*(IC))
								V(NS) = (*U(J))
								VLU(func()*int{y:=1;return &y}()) = (intrinsic.MIN(VLU(func()*int{y:=1;return &y}()), V(NS)))
								VLU(func()*int{y:=2;return &y}()) = (intrinsic.MAX(VLU(func()*int{y:=2;return &y}()), V(NS)))
								//!                   Plot min/max for graphics

								if (*PSMN) > (*V(NS)) {
									(*PSMN) = (*V(NS))
									XPSN(func()*int{y:=1;return &y}()) = (*XL(func()*int{y:=1;return &y}(), NS))
									XPSN(func()*int{y:=2;return &y}()) = (*XL(func()*int{y:=2;return &y}(), NS))
									XPSN(func()*int{y:=3;return &y}()) = (*XL(func()*int{y:=3;return &y}(), NS))
								}
								if (*PSMX) < (*V(NS)) {
									(*PSMX) = (*V(NS))
									XPSX(func()*int{y:=1;return &y}()) = (*XL(func()*int{y:=1;return &y}(), NS))
									XPSX(func()*int{y:=2;return &y}()) = (*XL(func()*int{y:=2;return &y}(), NS))
									XPSX(func()*int{y:=3;return &y}()) = (*XL(func()*int{y:=3;return &y}(), NS))
								}
							}
						}
					}
					//! i

					//!             Plot area contours

					if (*IUTOT) > 3 && (*PSTYP) >= 2 {
						//!               VEM 2-d elements

						if (*(IX)((*NEN)+7, N)) == -22 {
							(*KO) = (*(IX)((*NEN)+8, N))
							VEM_PLCON(KO, NC, NEL, XL, IPLT, ICL, V, VC, CONT)
							//!               Linear Triangle

						} else if (*NEL) == 3 {
							if *CONT {
								PLTECN(XL, V, VC, NC)
							} else {
								PLTCOR(func()*int{y:=3;return &y}(), ICL, V, VC, NC)
								PLTEFL(func()*int{y:=3;return &y}(), ICL, XL, V, VC, NC)
							}
							//!               Linear Quadrilateral

						} else if (*NEL) == 4 {
							PLTCOR(NEL, ICL, V, VC, NC)
							PLTQFL(ICL, XL, V, VC, NC, CONT)
							//!               Quadratic Triangle

						} else if (*NEL) == 6 || (*NEL) == 7 {
							for (*J) = 1; (*J) <= 4; (*J)++ {
								for (*I) = 1; (*I) <= 3; (*I)++ {
									(*II) = (*IT6(I, J))
									XQ(func()*int{y:=1;return &y}(), I) = (*XL(func()*int{y:=1;return &y}(), II))
									XQ(func()*int{y:=2;return &y}(), I) = (*XL(func()*int{y:=2;return &y}(), II))
									XQ(func()*int{y:=3;return &y}(), I) = (*XL(func()*int{y:=3;return &y}(), II))
									VQ(I) = (*V(II))
								}
								//! i

								XQ(func()*int{y:=1;return &y}(), func()*int{y:=4;return &y}()) = (*XQ(func()*int{y:=1;return &y}(), func()*int{y:=1;return &y}()))
								XQ(func()*int{y:=2;return &y}(), func()*int{y:=4;return &y}()) = (*XQ(func()*int{y:=2;return &y}(), func()*int{y:=1;return &y}()))
								XQ(func()*int{y:=3;return &y}(), func()*int{y:=4;return &y}()) = (*XQ(func()*int{y:=3;return &y}(), func()*int{y:=1;return &y}()))
								VQ(func()*int{y:=4;return &y}()) = (*VQ(func()*int{y:=1;return &y}()))
								if *CONT {
									PLTECN(XQ, VQ, VC, NC)
								} else {
									PLTCOR(func()*int{y:=3;return &y}(), ILQ, VQ, VC, NC)
									PLTEFL(func()*int{y:=3;return &y}(), ILQ, XQ, VQ, VC, NC)
								}
							}
							//! j

							//!               Quadratic quad

						} else if ((*NEL) == 8 && (*IUF) != 16) || (*NEL) == 9 {
							if (*NEL) == 8 {
								for (*I) = 1; (*I) <= 3; (*I)++ {
									XL(I, func()*int{y:=9;return &y}()) = -0.25e0*(XL(I, func()*int{y:=1;return &y}())+XL(I, func()*int{y:=2;return &y}())+XL(I, func()*int{y:=3;return &y}())+XL(I, func()*int{y:=4;return &y}())) + 0.50e0*(XL(I, func()*int{y:=5;return &y}())+XL(I, func()*int{y:=6;return &y}())+XL(I, func()*int{y:=7;return &y}())+XL(I, func()*int{y:=8;return &y}()))
								}
								//! i

								V(func()*int{y:=9;return &y}()) = -0.25e0*(V(func()*int{y:=1;return &y}())+V(func()*int{y:=2;return &y}())+V(func()*int{y:=3;return &y}())+V(func()*int{y:=4;return &y}())) + 0.50e0*(V(func()*int{y:=5;return &y}())+V(func()*int{y:=6;return &y}())+V(func()*int{y:=7;return &y}())+V(func()*int{y:=8;return &y}()))
								(*NEL) = 9
							} else {
								(*II) = (*(IX)(func()*int{y:=9;return &y}(), N))
								for (*I) = 1; (*I) <= (*(NDM)); (*I)++ {
									XL(I, func()*int{y:=9;return &y}()) = (*X(I, II))
								}
								//! i

								V(func()*int{y:=9;return &y}()) = (*U((*(NDF))*((*II)-1) + (*(IC))))
							}
							(*NEL) = 9
							PLTCOR(NEL, ICL, V, VC, NC)
							for (*J) = 1; (*J) <= 4; (*J)++ {
								for (*I) = 1; (*I) <= 4; (*I)++ {
									(*II) = (*IQ9(I, J))
									ILQ(I) = (*ICL(II))
									XQ(func()*int{y:=1;return &y}(), I) = (*XL(func()*int{y:=1;return &y}(), II))
									XQ(func()*int{y:=2;return &y}(), I) = (*XL(func()*int{y:=2;return &y}(), II))
									XQ(func()*int{y:=3;return &y}(), I) = (*XL(func()*int{y:=3;return &y}(), II))
									VQ(I) = (*V(II))
								}
								//! i

								PLTQFL(ILQ, XQ, VQ, VC, NC, CONT)
							}
							//! j

							//!               Cubic quad

						} else if (*NEL) == 16 {
							for (*J) = 13; (*J) <= 16; (*J)++ {
								(*II) = (*(IX)(J, N))
								for (*I) = 1; (*I) <= (*(NDM)); (*I)++ {
									XL(I, J) = (*X(I, II))
								}
								//! i

								V(J) = (*U((*(NDF))*((*II)-1) + (*(IC))))
							}
							//! j

							PLTCOR(NEL, ICL, V, VC, NC)
							for (*J) = 1; (*J) <= 9; (*J)++ {
								for (*I) = 1; (*I) <= 4; (*I)++ {
									(*II) = (*IQ16(I, J))
									ILQ(I) = (*ICL(II))
									XQ(func()*int{y:=1;return &y}(), I) = (*XL(func()*int{y:=1;return &y}(), II))
									XQ(func()*int{y:=2;return &y}(), I) = (*XL(func()*int{y:=2;return &y}(), II))
									XQ(func()*int{y:=3;return &y}(), I) = (*XL(func()*int{y:=3;return &y}(), II))
									VQ(I) = (*V(II))
								}
								//! i

								PLTQFL(ILQ, XQ, VQ, VC, NC, CONT)
							}
							//! j

							//!               If all else fails plot a quad

						} else {
							PLTCOR(NEL, ICL, V, VC, NC)
							PLTQFL(ICL, XL, V, VC, NC, CONT)
						}
						//!               Draw border around element

						PPPCOL(func()*int{y:=0;return &y}(), func()*int{y:=1;return &y}())
						if !(*CONT) && (*IPB) == 0 {
							PLOTL(XL(func()*int{y:=1;return &y}(), IPLT(func()*int{y:=1;return &y}())), XL(func()*int{y:=2;return &y}(), IPLT(func()*int{y:=1;return &y}())), XL(func()*int{y:=3;return &y}(), IPLT(func()*int{y:=1;return &y}())), func()*int{y:=3;return &y}())
							for (*I) = 2; (*I) <= (*IUF)-1; (*I)++ {
								(*J) = (*IPLT(I))
								if (*J) <= (*NEL) {
									PLOTL(XL(func()*int{y:=1;return &y}(), J), XL(func()*int{y:=2;return &y}(), J), XL(func()*int{y:=3;return &y}(), J), func()*int{y:=2;return &y}())
								}
							}
							//! i

							PLOTL(XL(func()*int{y:=1;return &y}(), IPLT(func()*int{y:=1;return &y}())), XL(func()*int{y:=2;return &y}(), IPLT(func()*int{y:=1;return &y}())), XL(func()*int{y:=3;return &y}(), IPLT(func()*int{y:=1;return &y}())), func()*int{y:=2;return &y}())
						}
						//!             Line elements

					} else {
						if (*(IX)(NEL, NE)) == (*(IX)((*NEL)-1, NE)) {
							(*NEL) = (*NEL) - 1
						}
						PLTLFL(NEL, XL, V, VC, NC)
					}
					//!           User plot types

				} else if (*PSTYP) < 0 {
					UFACELIB(PSTYP, NEL, IPU, NUMFAC)
					//!             Set values of vlu(1) and vlu(2)

					for (*NUMF) = 1; (*NUMF) <= (*NUMFAC); (*NUMF)++ {
						(*IUTOT) = 0
						VLU(func()*int{y:=1;return &y}()) = (*VMX)
						VLU(func()*int{y:=2;return &y}()) = (*VMN)
						for (*I) = 1; (*I) <= 4; (*I)++ {
							(*NS) = (*IPU(I, NUMF))
							if (*NS) <= (*NEL) {
								(*II) = (*(IX)(NS, N))
								if (*II) > 0 {
									(*IUTOT) = (*IUTOT) + 1
									XL(func()*int{y:=1;return &y}(), IUTOT) = (*X(func()*int{y:=1;return &y}(), II))
									XL(func()*int{y:=2;return &y}(), IUTOT) = (*X(func()*int{y:=2;return &y}(), II))
									if (*(NDM)) >= 3 {
										XL(func()*int{y:=3;return &y}(), IUTOT) = (*X(func()*int{y:=3;return &y}(), II))
									}
									(*J) = (*(NDF))*((*II)-1) + (*(IC))
									V(IUTOT) = (*U(J))
									VLU(func()*int{y:=1;return &y}()) = (intrinsic.MIN(VLU(func()*int{y:=1;return &y}()), V(IUTOT)))
									//!                     Plot min/max for graphics

									if (*PSMN) > (*V(IUTOT)) {
										(*PSMN) = (*V(IUTOT))
										XPSN(func()*int{y:=1;return &y}()) = (*XL(func()*int{y:=1;return &y}(), IUTOT))
										XPSN(func()*int{y:=2;return &y}()) = (*XL(func()*int{y:=2;return &y}(), IUTOT))
										XPSN(func()*int{y:=3;return &y}()) = (*XL(func()*int{y:=3;return &y}(), IUTOT))
									}
									if (*PSMX) < (*V(IUTOT)) {
										(*PSMX) = (*V(IUTOT))
										XPSX(func()*int{y:=1;return &y}()) = (*XL(func()*int{y:=1;return &y}(), IUTOT))
										XPSX(func()*int{y:=2;return &y}()) = (*XL(func()*int{y:=2;return &y}(), IUTOT))
										XPSX(func()*int{y:=3;return &y}()) = (*XL(func()*int{y:=3;return &y}(), IUTOT))
									}
								}
							}
						}
						//! i

						for (*I) = 1; (*I) <= 3; (*I)++ {
							XL(I, (*IUTOT)+1) = (*XL(I, func()*int{y:=1;return &y}()))
						}
						//! i

						PLTCOR(func()*int{y:=4;return &y}(), ICL, V, VC, NC)
						PLTQFL(ICL, XL, V, VC, NC, CONT)
					}
					//! numf

					//!             Draw border around element

					PPPCOL(func()*int{y:=0;return &y}(), func()*int{y:=1;return &y}())
					(*IUF) = (*INORD(MA))
					if !(*CONT) && (*IPB) == 0 {
						PLOTL(X(func()*int{y:=1;return &y}(), (IX)(IPORD(func()*int{y:=1;return &y}(), MA), N)), X(func()*int{y:=2;return &y}(), (IX)(IPORD(func()*int{y:=1;return &y}(), MA), N)), X(func()*int{y:=3;return &y}(), (IX)(IPORD(func()*int{y:=1;return &y}(), MA), N)), func()*int{y:=3;return &y}())
						for (*I) = 2; (*I) <= (*IUF); (*I)++ {
							PLOTL(X(func()*int{y:=1;return &y}(), (IX)(IPORD(I, MA), N)), X(func()*int{y:=2;return &y}(), (IX)(IPORD(I, MA), N)), X(func()*int{y:=3;return &y}(), (IX)(IPORD(I, MA), N)), func()*int{y:=2;return &y}())
						}
						//! i

						PLOTL(X(func()*int{y:=1;return &y}(), (IX)(IPORD(func()*int{y:=1;return &y}(), MA), N)), X(func()*int{y:=2;return &y}(), (IX)(IPORD(func()*int{y:=1;return &y}(), MA), N)), X(func()*int{y:=3;return &y}(), (IX)(IPORD(func()*int{y:=1;return &y}(), MA), N)), func()*int{y:=2;return &y}())
					}
				}
			}
		}
	}
	//! ne

	//!     Put on labels

	if *LABL {
		if *CONT {
			PLTCTX(VC, (LC), NLABI, NC, (MMC))
		} else {
			PLTFTX(VC, -(*(MC)), (MMC))
		}
		(*LABL) = false
	}
	//!     Formats

	//!1000  format(a)

//Label2000:
	;
	// Unused by f4go :  !1000  format(a)
 2000 format ( "   ------ Contour Values for Plot ------" / ( 3 X , 5e15 .6 ) )
//Label2001:
	;
	// Unused by f4go :  2001 format ( " Input" , I3 , " Contour Values for Plot - 8 Values/Line" )
//Label2002:
	;
	// Unused by f4go :  2002 format ( " Input number for first contour label > " , $ )
//Label2005:
	;
	// Unused by f4go :  2005 format ( " ** ERROR ** No plot - all zero values" )
//Label2006:
	;
	// Unused by f4go :  2006 format ( " Input values correct? (y or n, c = cancel) > " , $ )
//Label2007:
	;
	// Unused by f4go :  2007 format ( " ** WARNING ** Initial label reset to fit screen" )
//Label2008:
	;
	// Unused by f4go :  2008 format ( " Input Min/Max (Default:" , 1 P , E9 . 2 , "/" , 1 P , E9 . 2 , "): >" , $ )
//Label2009:
	;
	// Unused by f4go :  2009 format ( 3 X , ">" , $ )
}
