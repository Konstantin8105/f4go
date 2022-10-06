package main

import "github.com/Konstantin8105/f4go/intrinsic"

type MEMORY struct {
	PDATRI struct {
		XMX *float64
		XMN *float64
		YMX *float64
		YMN *float64
	}
	PRANGE struct {
		RANGMN *float64
		RANGMX *float64
		RANGFL *bool
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
	PDATA4 struct {
		XMIN *float64
		XMAX *float64
		NZM1 *int
		NZM2 *int
		NFAC *int
		NFMX *int
		PDF  *int
	}
	POINTER struct {
		NP *int64
		UP *int64
	}
	NPOINTS struct {
		NPUD *int64
		NPID *int64
		NPIX *int64
		NPUU *int64
		NPXX *int64
		NPER *int64
		NPNP *int64
		NPEV *int64
		NPRN *int64
		NPTY *int64
	}
	PRMPTD struct {
		PROMPT *bool
		DEFALT *bool
		PSCOLR *bool
		PSREVS *bool
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
	COMINT struct {
		FINDEX *int
	}
	HDATAM struct {
		PLTMFL *bool
		NHMAX  *int
		NH3MAX *int
		HFLGU  *bool
		H3FLGU *bool
		NDFLG  *bool
	}
	IOFILE struct {
		KEEPFL *bool
		IOR    *int
		IOW    *int
	}
	PPOINTS struct {
		PLIX *int64
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
	COMFIL struct {
		FINP   *[   ]byte
		FOUT   *[   ]byte
		FRES   *[   ]byte
		FSAV   *[   ]byte
		FPLT   *[   ]byte
		FINCLD *[   ]byte
	}
	PSDAT1 struct {
		PSMX *float64
		PSMN *float64
		XPSX *float64
		XPSN *float64
	}
	RPDATA struct {
		RMN *float64
		RMX *float64
	}
	COMREC struct {
		RECORD *[   ]byte
	}
	PBODY struct {
		MAPLT *int
	}
}

var COMMON MEMORY
//!$Id:$
func PLTELC(X *int, IE *int, IX *int, IP *int, DT *int, ST *int, VV *int, NIE *int, NDM *int, NDF *int, NEN1 *int, IC *int, MC *int, LC *int, MMC *int, LABEL *bool) {
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
	NHMAX := new(int)
	NH3MAX := new(int)
	HFLGU := new(bool)
	H3FLGU := new(bool)
	NDFLG := new(bool)
	COMMON.HDATAM.NDFLG = new(float64)
	COMMON.HDATAM.H3FLGU = new(float64)
	COMMON.HDATAM.HFLGU = new(float64)
	COMMON.HDATAM.NH3MAX = new(float64)
	COMMON.HDATAM.NHMAX = new(int)
	PLTMFL := new(bool)
	COMMON.HDATAM.PLTMFL = new(int)
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
	XMX := new(float64)
	XMN := new(float64)
	YMX := new(float64)
	YMN := new(float64)
	COMMON.PDATRI.YMN = new(float64)
	COMMON.PDATRI.YMX = new(float64)
	COMMON.PDATRI.XMN = new(float64)
	COMMON.PDATRI.XMX = new(int)
	NUM_NPS := new(int)
	NUM_UPS := new(int)
	NP := new(int64)
	UP := new(int64)
	COMMON.POINTER.UP = func() *[]float64 {
		arr := make([]float64, 200)
		return &arr
	}()
	COMMON.POINTER.NP = func() *[]int {
		arr := make([]int, 400)
		return &arr
	}()
	NPID := new(int64)
	NPIX := new(int64)
	NPUU := new(int64)
	NPXX := new(int64)
	NPER := new(int64)
	NPNP := new(int64)
	NPEV := new(int64)
	NPRN := new(int64)
	NPTY := new(int64)
	COMMON.NPOINTS.NPTY = new(float64)
	COMMON.NPOINTS.NPRN = new(float64)
	COMMON.NPOINTS.NPEV = new(float64)
	COMMON.NPOINTS.NPNP = new(float64)
	COMMON.NPOINTS.NPER = new(float64)
	COMMON.NPOINTS.NPXX = new(float64)
	COMMON.NPOINTS.NPUU = new(float64)
	COMMON.NPOINTS.NPIX = new(float64)
	COMMON.NPOINTS.NPID = new(int)
	NPUD := new(int64)
	COMMON.NPOINTS.NPUD = new(int)
	PLIX := new(int64)
	COMMON.PPOINTS.PLIX = new(int)
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
	TVC := new(bool)
	VFLG := new(bool)
	ERRCK := new(bool)
	CONT := new(bool)
	CINPUT := new(bool)
	PINPUT := new(bool)
	LABL := new(bool)
	ICOLOR := new(int)
	I := new(int)
	N := new(int)
	MA := new(int)
	MAOLD := new(int)
	NC := new(int)
	NNC := new(int)
	NERR := new(int)
	NLABI := new(int)
	IPLT := new(int)
	IU := new(int)
	IUTOT := new(int)
	NS := new(int)
	II := new(int)
	NE := new(int)
	KIND := func() *[]float64 {
		arr := make([]float64, -1)
		return &arr
	}()
	KIND := func() *[]float64 {
		arr := make([]float64, -1)
		return &arr
	}()
	KIND := func() *[]float64 {
		arr := make([]float64, -1)
		return &arr
	}()
	//!      * * F E A P * * A Finite Element Analysis Program

	//!....  Copyright (c) 1984-2021: Regents of the University of California

	//!                               All rights reserved

	//!-----[--.----+----.----+----.-----------------------------------------]

	//!      Purpose: Plot of element contours: No inter-element smoothing

	//!      Inputs:

	//!         x(ndm,*)  - Nodal coordinates of mesh

	//!         ie(nie,*) - Assembly data for material sets

	//!         ix(nen1,*)- Element nodal connections

	//!         ip(*)     - Sorted element order

	//!         dt(*)     - Storage for element node projection

	//!         st(numnp*)- Storage for element node projection

	//!         vv(nen,*) - Element projected value

	//!         nie       - Dimension of ie array

	//!         ndm       - Dimension of x array

	//!         ndf       - Number dof/node

	//!         nen1      - Dimension of ix array

	//!         ic        - Component value to plot

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
	NHMAX = COMMON.HDATAM.NHMAX
	NH3MAX = COMMON.HDATAM.NH3MAX
	HFLGU = COMMON.HDATAM.HFLGU
	H3FLGU = COMMON.HDATAM.H3FLGU
	NDFLG = COMMON.HDATAM.NDFLG
	PLTMFL = COMMON.HDATAM.PLTMFL
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
	XMX = COMMON.PDATRI.XMX
	XMN = COMMON.PDATRI.XMN
	YMX = COMMON.PDATRI.YMX
	YMN = COMMON.PDATRI.YMN
	(*NUM_NPS) = 400
	(*NUM_UPS) = 200
	//! int8

	NP = COMMON.POINTER.NP
	UP = COMMON.POINTER.UP
	NPID = COMMON.NPOINTS.NPID
	NPIX = COMMON.NPOINTS.NPIX
	NPUU = COMMON.NPOINTS.NPUU
	NPXX = COMMON.NPOINTS.NPXX
	NPER = COMMON.NPOINTS.NPER
	NPNP = COMMON.NPOINTS.NPNP
	NPEV = COMMON.NPOINTS.NPEV
	NPRN = COMMON.NPOINTS.NPRN
	NPTY = COMMON.NPOINTS.NPTY
	NPUD = COMMON.NPOINTS.NPUD
	PLIX = COMMON.PPOINTS.PLIX
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
	//F4GO: NOT IMPLEMENTED :real ( KIND = 8 ) :: DX1 , VMX , VMN
	//F4GO: NOT IMPLEMENTED :real ( KIND = 8 ) :: XL ( 3 , 29 ) , X ( NDM , * ) , DT ( * ) , ST ( NUMNP , * ) , VV ( NEN , * )
	//F4GO: NOT IMPLEMENTED :real ( KIND = 8 ) :: V ( 29 ) , VC ( 12 ) , VLU ( 2 )
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
		//!20     read (*,1000,err=21,end=22) y

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
	//!     Compute projected max/min values

	(*VMN) = (*ST(func()*int{y:=1;return &y}(), (LC)))
	(*VMX) = (*ST(func()*int{y:=1;return &y}(), (LC)))
	for (*I) = 1; (*I) <= (*NUMNP); (*I)++ {
		(*VMN) = (intrinsic.MIN((*VMN), ST(I, (LC))))
		(*VMX) = (intrinsic.MAX((*VMX), ST(I, (LC))))
		ST(I, (LC)) = 0.0e0
		DT(I) = 0.0e0
	}
	if (*VMN) == 0.0e0 && (*VMX) == 0.0e0 {
		// Unused by f4go :  write ( IOW , 2005 )
		fmt.Println("WRITE SOMETHING")
		if (*IOR) < 0 {
			// Unused by f4go :  if ( IOR .lt. 0 ) write ( * , 2005 )
			fmt.Println("WRITE SOMETHING")
		}
		return
	}
	//!     Compute element nodal values

	for (*N) = 1; (*N) <= (*NUMEL); (*N)++ {
		//!       Compute value in element

		(*NE) = (*N)
		(*PLTMFL) = true
		FORMFE(NP(func()*int{y:=40;return &y}()), NP(func()*int{y:=40;return &y}()), NP(func()*int{y:=40;return &y}()), NP(func()*int{y:=40;return &y}()), &(false), &(false), &(false), func()*int{y:=8;return &y}(), NE, NE, func()*int{y:=1;return &y}())
		(*PLTMFL) = false
		//!       Do projection

		for (*I) = 1; (*I) <= (*NEN); (*I)++ {
			if (*(IX)(I, N)) > 0 {
				if (*DT((IX)(I, N))) != 0.0e0 {
					VV(I, N) = ST((IX)(I, N), (LC)) / DT((IX)(I, N))
				}
			}
		}
		for (*I) = 1; (*I) <= (*NEN); (*I)++ {
			if (*(IX)(I, N)) > 0 {
				ST((IX)(I, N), (LC)) = 0.0e0
				DT((IX)(I, N)) = 0.0e0
			}
		}
	}
	//!     Open plot and find max/min of plot variable

	PLOPEN()
	(*XMX) = (*X(func()*int{y:=1;return &y}(), func()*int{y:=1;return &y}()))
	(*YMX) = (*X(func()*int{y:=2;return &y}(), func()*int{y:=1;return &y}()))
	(*XMN) = (*X(func()*int{y:=1;return &y}(), func()*int{y:=1;return &y}()))
	(*YMN) = (*X(func()*int{y:=2;return &y}(), func()*int{y:=1;return &y}()))
	for (*I) = 1; (*I) <= (*NUMNP); (*I)++ {
		(*XMX) = (intrinsic.MAX(X(func()*int{y:=1;return &y}(), I), (*XMX)))
		(*YMX) = (intrinsic.MAX(X(func()*int{y:=2;return &y}(), I), (*YMX)))
		(*XMN) = (intrinsic.MIN(X(func()*int{y:=1;return &y}(), I), (*XMN)))
		(*YMN) = (intrinsic.MIN(X(func()*int{y:=2;return &y}(), I), (*YMN)))
	}
	if (*XMX) != (*XMN) {
		(*XMX) = 8.2e0 / ((*XMX) - (*XMN))
	}
	if (*YMX) != (*YMN) {
		(*YMX) = 8.2e0 / ((*YMX) - (*YMN))
	}
	//!     Loop through elements

	PZERO(XL, 3*(*NEN))
	(*(IC)) = 1
	(*MAOLD) = -1
	(*PSMX) = (*VMN) - 1.
	(*PSMN) = (*VMX) + 1.
	for (*NE) = 1; (*NE) <= (*NFAC); (*NE)++ {
		//!       Set element based on symmetry condition

		(*N) = (*(IP)(NE))
		//!       If region is active: Plot material number maplt

		//!                            - all if maplt = 0; ma > 0 active

		if (*N) > 0 {
			(*MA) = (*(IX)((NEN1), N))
			if ((IX)((*(NEN1))-1, N) >= 0) && ((*MA) > 0 && (*MAPLT) == 0) || (*MA) == (*MAPLT) {
				(*MA) = (*(IE)((*(NIE))-1, (IX)((NEN1), N)))
				//!           Get plot order for each element

				PLTORD((IX)(func()*int{y:=1;return &y}(), N), MA, IU, IPLT)
				(*IUTOT) = (*IU)
				//!           Set values of vlu(1) and vlu(2)

				VLU(func()*int{y:=1;return &y}()) = (*VMX)
				VLU(func()*int{y:=2;return &y}()) = (*VMN)
				(*NS) = 0
				for (*I) = 1; (*I) <= (*IU); (*I)++ {
					(*II) = (*(IX)(IPLT(I), N))
					if (*II) > 0 {
						(*NS) = (*NS) + 1
						XL(func()*int{y:=1;return &y}(), NS) = (*X(func()*int{y:=1;return &y}(), II))
						XL(func()*int{y:=2;return &y}(), NS) = (*X(func()*int{y:=2;return &y}(), II))
						if (*(NDM)) >= 3 {
							XL(func()*int{y:=3;return &y}(), NS) = (*X(func()*int{y:=3;return &y}(), II))
						}
						V(NS) = (*VV(IPLT(I), N))
						VLU(func()*int{y:=1;return &y}()) = (intrinsic.MIN(VLU(func()*int{y:=1;return &y}()), V(NS)))
						VLU(func()*int{y:=2;return &y}()) = (intrinsic.MAX(VLU(func()*int{y:=2;return &y}()), V(NS)))
						//!               Plot min/max for graphics

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
				if (*NS) > 3 {
					if (*NEN) >= 9 && (*IUTOT) == 9 {
						if (*(IX)(func()*int{y:=9;return &y}(), N)) > 0 {
							V(func()*int{y:=29;return &y}()) = (*VV(func()*int{y:=9;return &y}(), N))
						}
					} else if (*NEN) >= 7 && (*IUTOT) == 7 {
						if (*(IX)(func()*int{y:=7;return &y}(), N)) > 0 {
							V(func()*int{y:=29;return &y}()) = (*VV(func()*int{y:=7;return &y}(), N))
						}
					}
					PLTRIS((IC), NC, N, NS, IUTOT, (NDM), (NDF), NEN, (NEN1), NLABI, ICOLOR, (IX), X, XL, V, VC, DX1, VLU(func()*int{y:=1;return &y}()), VLU(func()*int{y:=2;return &y}()), TVC, CONT, VFLG)
				} else {
					PLTLFL(NS, XL, V, VC, NC)
				}
			}
		}
		//! n > 0

	}
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
 2000 format ( "   ------ Contour Values for Plot ------" / ( 3 X , 1 P , 5e15 .6 ) )
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
