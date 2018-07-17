      SUBROUTINE  SYMSF (FONT,SWCHAR)
C     (Select Font)
C     Select  one  of  the  Hershey  fonts  for  subsequent  text
C     plotting.  Up to five fonts may  be in effect at one  time.
C     The last selected is  the default one.   If more than  five
C     fonts are requested, the sixth will replace the first,  the
C     seventh the second, and  so on in  a cyclic fashion.   This
C     restriction  is  purely  dimensional,  and  may  easily  be
C     changed  if  required.   The  case  switch  characters  are
C     initialized to  "<"  and  ">"  for  to-upper  and  to-lower
C     respectively.    The   backspace   character   switch    is
C     initialized to 0, suppressing the backspace facility.   The
C     arguments are:
C
C     FONT(*).....5-character string selecting font (see below).
C     SWCHAR(*)...Single  character (e.g. 1H=)  used  as a switch
C                 character to return to this font from  another.
C                 It will be interpreted as a switch character if
C                 it occurs only  once.  However, two  successive
C                 switch characters  for a  single font  will  be
C                 collapsed to a single character and will not be
C                 interpreted as a font switch.  A blank or  zero
C                 value indicates  that  no switch  character  is
C                 selected.
C
C     The fonts are selected by  a five-character string made  up
C     of a  2-character case  specification, a  2-character  type
C     specification, and a 1-character variant specification,  as
C     follows:
C
C     Case: UC - Upper Case
C           LC - Lower Case
C
C     Type: KR - Cartographic Roman (9)
C           KG - Cartographic Greek (9)
C           IR - Indexical Roman (13)
C           II - Indexical Roman Italic (13)
C           IG - Indexical Greek (13)
C           SA - Simplex ASCII (15)
C           BA - Block ASCII (15)
C           SR - Simplex Roman (21)
C           SS - Simplex Roman Script (21)
C           SG - Simplex Greek (21)
C           CR - Complex Normal Roman (21)
C           CI - Complex Normal Roman Italic (21)
C           CG - Complex Normal Greek (21)
C           CS - Complex Script (21)
C           DR - Duplex Roman (21)
C           TR - Triplex Roman (21)
C           GE - Gothic English (21)
C           GI - Gothic Italian (21)
C           GG - Gothic German (21)
C           CC - Complex Cyrillic (21)
C
C     Variant: 1 - Principal
C              2 - Secondary
C              3 - Tertiary
C              4 - Quaternary
C
C     Selector letters may be  either upper- or lower-case.   The
C     case specification is arranged  such that if upper-case  is
C     requested, upper-case text will be mapped into  upper-case,
C     and  lower-case   into   lower-case.   If   lower-case   is
C     requested, both upper-  and lower-case  letters are  mapped
C     into lower case.  The four  variants are provided to  allow
C     representation of  special  characters within  the  limited
C     FORTRAN set.  The Gothic and  Cyrillic fonts have only  two
C     variants available.  Requests for variants  3 or 4 will  be
C     reduced to  variant  2.   The ASCII  fonts  have  only  one
C     variant, and  requests for  variants  2, 3,  or 4  will  be
C     reduced to variant 1.
C
C     The numbers (9),  (13), (15), and  (21) following the  type
C     indicate the height of the characters in raster units.  The
C     spacing between lines of text is conventionally measured by
C     the printer's  unit  "em",  giving the  distance  from  the
C     bottom of one line of type to the bottom of the next  line.
C     It should be  21 raster  units for indexical  size, and  32
C     raster units for normal size.
C
C     If any of the three parts  of the font specification is  in
C     error, a message  will be  issued, and a  default for  that
C     part will be assumed.  The default corresponds to  "UCTR1".
C     (01-APR-83)
C  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C     EXTERNAL REFERENCES (FUNCTION,SUBROUTINE,COMMON)
C
C     EXTERNAL REFS       ERRAT,       ERRCK,       ERRMS,       KARASC
C     EXTERNAL REFS       KARCM2,      KARUC,       KARUPK,      MIN0
C     EXTERNAL REFS       MOD
C
C  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C     EXTERNAL FUNCTIONS AND SUBROUTINES
C
      INTEGER             KARASC,      KARCM2,      KARUC
C  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C     INTRINSIC FUNCTIONS
C
      INTEGER             MIN0,        MOD
C  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C     Working variables
C
      INTEGER             FCASE,       FONT(1),     FTYPE,       FVAR
      INTEGER             I,           IDIG,        ILC,         IUC
      INTEGER             LOC0,        LOC0SV,      LOCLC,       LOCUC
      INTEGER             NOCHAR,      NUL,         SWCHAR(1)
      LOGICAL             ERROR
C
C     Font type case selectors
C
      INTEGER             BA
      INTEGER             CC,          CG,          CI,          CR
      INTEGER             CS,          DR,          GE,          GG
      INTEGER             GI,          IG,          II,          IR
      INTEGER             KG,          KR,          SA,          SG
      INTEGER             SR,          SS,          TR
C
C     Roman alphabet symbols in upper-case ASCII.
C
      INTEGER             UCA,         UCB,         UCC,         UCD
      INTEGER             UCE,         UCF,         UCG,         UCH
      INTEGER             UCI,         UCJ,         UCK,         UCL
      INTEGER             UCM,         UCN,         UCO,         UCP
      INTEGER             UCQ,         UCR,         UCS,         UCT
      INTEGER             UCU,         UCV,         UCW,         UCX
      INTEGER             UCY,         UCZ
C
C     Roman alphabet symbols in lower-case ASCII.
C
      INTEGER             LCA,         LCB,         LCC,         LCD
      INTEGER             LCE,         LCF,         LCG,         LCH
      INTEGER             LCI,         LCJ,         LCK,         LCL
      INTEGER             LCM,         LCN,         LCO,         LCP
      INTEGER             LCQ,         LCR,         LCS,         LCT
      INTEGER             LCU,         LCV,         LCW,         LCX
      INTEGER             LCY,         LCZ
C
C     Greek alphabet symbols ordered relative to first letter.
C
      INTEGER             ALPHA,       BETA,        CHI,         DELTA
      INTEGER             EPSLON,      ETA,         GAMMA,       IOTA
      INTEGER             KAPPA,       LAMBDA,      MU,          NU
      INTEGER             OMCRON,      OMEGA,       PHI,         PI
      INTEGER             PSI,         RHO,         SIGMA,       TAU
      INTEGER             THETA,       UPSLON,      XI,          ZETA
C
C     Cyrillic alphabet ordered relative to first letter.
C
      INTEGER             CYA,         CYB,         CYCHE,       CYD
      INTEGER             CYE,         CYEE,        CYEEK,       CYF
      INTEGER             CYG,         CYK,         CYKHA,       CYL
      INTEGER             CYM,         CYMZNK,      CYN,         CYO
      INTEGER             CYOO,        CYP,         CYR,         CYS
      INTEGER             CYSH,        CYSHCH,      CYT,         CYTSE
      INTEGER             CYTZNK,      CYV,         CYYA,        CYYE
      INTEGER             CYYIRI,      CYYOO,       CYZ,         CYZHE
C
C     ASCII special characters
C
      INTEGER             ACCENT,      AMPSND,      AT,          CARET
      INTEGER             COLON,       COMMA,       DEL,         DOLLAR
      INTEGER             DQUOTE,      EQUALS,      EXCLPT,      LANGLE
      INTEGER             LBRACE,      LBRAKT,      LPAREN,      MINUS
      INTEGER             NUMBER,      PERCNT,      PERIOD,      PLUS
      INTEGER             QUERY,       RANGLE,      RBRACE,      RBRAKT
      INTEGER             RPAREN,      RSLANT,      SCOLON,      SLASH
      INTEGER             SPACE,       SQUOTE,      STAR,        TILDE
      INTEGER             USCORE,      VBAR
C
C     COMMON declarations
C
C ----------------------------------------------------------------------
C          C O R E   G R A P H I C S   S Y S T E M   T E X T
C            C U R R E N T   F O N T   P A R A M E T E R S
C                       C O M M O N   B L O C K
C
C     CASESW:        Current temporary font case (1=UC, 2=LC)
C     KFONT:         Current font table index
C     MAXFNT:        Maximum font index
C     NFONT:         Index of most recent font table established
C     NFUSED:        Maximum number of font tables in use
C
      INTEGER          CASESW,      KFONT,       MAXFNT,      NFONT
      INTEGER          NFUSED
      COMMON /SYM02 /  CASESW,      KFONT,       MAXFNT,      NFONT
      COMMON /SYM02 /  NFUSED
C ----------------------------------------------------------------------
C          C O R E   G R A P H I C S   S Y S T E M   T E X T
C                          F O N T   D A T A
C                       C O M M O N   B L O C K
C
C     ASCII(*,*):    Table of Hershey characters assigned to
C                    ASCII values
C     BSWTCH(*):     Backspace switch ASCII character numbers for
C                    each font
C     FONTID(*):     Packed integer font identification for each font
C     FONTNM(*,*):   Unpacked Hollerith font name for each font
C     FSWTCH(*):     Font switch ASCII character numbers for each
C                    font
C     LSWTCH(*):     Lower-case switch ASCII character numbers
C                    for each font
C     USWTCH(*):     Upper-case switch ASCII character numbers
C                    for each font
C
      INTEGER          ASCII,       BSWTCH,      FONTID,      FONTNM
      INTEGER          FSWTCH,      LSWTCH,      USWTCH
      COMMON /SYM03 /  ASCII(96,5), BSWTCH(5),   FONTID(5)
      COMMON /SYM03 /  FONTNM(5,5), FSWTCH(5),   LSWTCH(5),   USWTCH(5)
C
C     Roman alphabet symbols in upper-case ASCII.
C
      DATA UCA,UCB,UCC,UCD,UCE,UCF,UCG,UCH,UCI,UCJ,UCK,UCL,UCM,UCN,
     XUCO,UCP,UCQ,UCR,UCS,UCT,UCU,UCV,UCW,UCX,UCY,UCZ/
     X65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78,
     X79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90/
C
C     Roman alphabet symbols in lower-case ASCII.
C
      DATA LCA,LCB,LCC,LCD,LCE,LCF,LCG,LCH,LCI,LCJ,LCK,LCL,LCM,LCN,
     XLCO,LCP,LCQ,LCR,LCS,LCT,LCU,LCV,LCW,LCX,LCY,LCZ/
     X97, 98, 99,100,101,102,103,104,105,106,107,108,109,110,          1
     X11,112,113,114,115,116,117,118,119,120,121,122/
C
C     Greek alphabet symbols ordered relative to first letter.
C
      DATA ALPHA,BETA,GAMMA,DELTA,EPSLON,ZETA,ETA,THETA,IOTA,KAPPA,
     XLAMBDA,MU,NU,XI,OMCRON,PI,RHO,SIGMA,TAU,UPSLON,PHI,CHI,PSI,
     XOMEGA/
     X0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,
     X23/
C
C     Cyrillic alphabet symbols ordered relative to first letter.
C
      DATA CYA,CYB,CYV,CYG,CYD,CYYE,CYZHE,CYZ,CYEE,CYEEK,CYK,
     XCYL,CYM,CYN,CYO,CYP,CYR,CYS,CYT,CYOO,CYF,CYKHA,
     XCYTSE,CYCHE,CYSH,CYSHCH,CYTZNK,CYYIRI,CYMZNK,CYE,CYYOO,CYYA/
     X0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,
     X23,24,25,26,27,28,29,30,31/
C
C     ASCII special characters
C
      DATA SPACE,EXCLPT,DQUOTE,NUMBER,DOLLAR,PERCNT,AMPSND,
     XSQUOTE,LPAREN,RPAREN,STAR,PLUS,COMMA,MINUS,PERIOD,
     XSLASH,COLON,SCOLON,LANGLE,EQUALS,RANGLE,QUERY,AT,
     XLBRAKT,RSLANT,RBRAKT,CARET,USCORE,ACCENT,LBRACE,
     XVBAR,RBRACE,TILDE,DEL/
     X32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,
     X58,59,60,61,62,63,64,
     X91,92,93,94,95,
     X96,123,124,125,126,127/
      DATA NUL/0/
C
C     Font type case switch values.
C
      DATA KR/1/, KG/2/, IR/3/, II/4/, IG/5/, SR/6/, SS/7/, SG/8/
      DATA CR/9/, CI/10/, CG/11/, CS/12/, DR/13/, TR/14/, GE/15/
      DATA GI/16/, GG/17/, CC/18/, SA/19/, BA/20/
C
      ASSIGN 20001 TO NPR001
      GO TO 30001
20001 ASSIGN 20002 TO NPR002
      GO TO 30002
20002 ASSIGN 20003 TO NPR003
      GO TO 30003
20003 ASSIGN 20004 TO NPR004
      GO TO 30004
20004 IF (.NOT.(ERROR)) GO TO 20005
      ASSIGN 20005 TO NPR005
      GO TO 30005
20005 ASSIGN 20006 TO NPR006
      GO TO 30006
C
20006 RETURN
C
C-----------------------------------------------------------------------
C---- PROCEDURE (Alphanumerics)
30007 IF (FCASE .EQ. 2) LOCUC = LOCLC
      I =1
      GO TO 20008
20007 I =I +1
20008 IF ((26-I ).LT.0) GO TO 20009
      IUC = I + 64 - 31
      ASCII(IUC,NFONT) = LOCUC + I - 1
      ILC = I + 96 - 31
      ASCII(ILC,NFONT) = LOCLC + I - 1
      GO TO 20007
20009 ASSIGN 20011 TO NPR008
      GO TO 30008
20011 GO TO NPR007, (20039,20055,20058,20077,20085,20088,20094,20131,201
     X53,20168,20249,20252,20255)
C
C-----------------------------------------------------------------------
C---- PROCEDURE (BA - Block ASCII)
30009 LOCUC = 1765
      LOCLC = 1797
      I =32
      GO TO 20013
20012 I =I +1
20013 IF ((127-I ).LT.0) GO TO 20014
      ASCII(I-31,NFONT) = 1700 + I
      GO TO 20012
20014 IF (.NOT.(FCASE .EQ. 2)) GO TO 20016
      I =65
      GO TO 20020
20019 I =I +1
20020 IF ((90-I ).LT.0) GO TO 20021
      ASCII(I-31,NFONT) = 1700 + I + 32
      GO TO 20019
20021 CONTINUE
20016 GO TO 20225
C
C-----------------------------------------------------------------------
C---- PROCEDURE (Cartographic Special Characters)
30010 NOCHAR = LOC0 - 1
      ASSIGN 20023 TO NPR011
      GO TO 30011
20023 ASCII(SPACE -31,NFONT) = LOC0 - 1
      ASCII(EXCLPT-31,NFONT) = LOC0 + 14
      ASCII(DQUOTE-31,NFONT) = LOC0 + 17
      ASCII(NUMBER-31,NFONT) = LOC0 + 33
      ASCII(AMPSND-31,NFONT) = LOC0 + 34
      ASCII(SQUOTE-31,NFONT) = LOC0 + 16
      ASCII(COLON -31,NFONT) = LOC0 + 12
      ASCII(SCOLON-31,NFONT) = LOC0 + 13
      ASCII(QUERY -31,NFONT) = LOC0 + 15
      ASCII(VBAR  -31,NFONT) = LOC0 + 23
      GO TO NPR010, (20163,20166,20239)
C
C-----------------------------------------------------------------------
C---- PROCEDURE (Cartographic Standard Signs)
30012 NX0028=FVAR
      IF (NX0028.LT.1.OR.NX0028.GT.4) GO TO 20028
      GO TO (20024,20025,20026,20027), NX0028
20024 ASCII(COMMA -31,NFONT) =  211
      ASCII(PERIOD-31,NFONT) =  210
      ASCII(LPAREN-31,NFONT) =  221
      ASCII(RPAREN-31,NFONT) =  222
      ASCII(MINUS -31,NFONT) =  224
      ASCII(PLUS  -31,NFONT) =  225
      ASCII(STAR  -31,NFONT) =  228
      ASCII(SLASH -31,NFONT) =  220
      ASCII(EQUALS-31,NFONT) =  226
      ASCII(DOLLAR-31,NFONT) =  219
      ASCII(AT    -31,NFONT) = 1273
      GO TO 20029
20025 ASCII(COMMA -31,NFONT) =  213
      ASCII(PERIOD-31,NFONT) =  215
      ASCII(LPAREN-31,NFONT) =  221
      ASCII(RPAREN-31,NFONT) =  222
      ASCII(MINUS -31,NFONT) =  224
      ASCII(PLUS  -31,NFONT) =  225
      ASCII(STAR  -31,NFONT) =  229
      ASCII(SLASH -31,NFONT) =  220
      ASCII(EQUALS-31,NFONT) =  226
      ASCII(DOLLAR-31,NFONT) =  233
      ASCII(AT    -31,NFONT) =  232
      GO TO 20029
20026 ASCII(COMMA -31,NFONT) =  212
      ASCII(PERIOD-31,NFONT) =  214
      ASCII(LPAREN-31,NFONT) =  221
      ASCII(RPAREN-31,NFONT) =  222
      ASCII(MINUS -31,NFONT) =  224
      ASCII(PLUS  -31,NFONT) =  225
      ASCII(STAR  -31,NFONT) =  227
      ASCII(SLASH -31,NFONT) =  220
      ASCII(EQUALS-31,NFONT) =  226
      ASCII(DOLLAR-31,NFONT) =  223
      ASCII(AT    -31,NFONT) =  230
      GO TO 20029
20027 ASCII(COMMA -31,NFONT) =  216
      ASCII(PERIOD-31,NFONT) =  217
      ASCII(LPAREN-31,NFONT) =  221
      ASCII(RPAREN-31,NFONT) =  222
      ASCII(MINUS -31,NFONT) =  224
      ASCII(PLUS  -31,NFONT) =  225
      ASCII(STAR  -31,NFONT) =  218
      ASCII(SLASH -31,NFONT) =  220
      ASCII(EQUALS-31,NFONT) =  226
      ASCII(DOLLAR-31,NFONT) =  235
      ASCII(AT    -31,NFONT) =  231
      GO TO 20029
20028 ASSIGN 20030 TO NPR013
      GO TO 30013
20030 CONTINUE
20029 GO TO NPR012, (20164,20167)
C
C-----------------------------------------------------------------------
C---- PROCEDURE (CC - Complex Cyrillic)
30014 FVAR = MIN0(FVAR,2)
      LOCUC = 2801
      LOCLC = 2901
      LOC0 = 2200
      ASSIGN 20031 TO NPR015
      GO TO 30015
20031 ASSIGN 20032 TO NPR016
      GO TO 30016
20032 ASSIGN 20033 TO NPR017
      GO TO 30017
20033 GO TO 20223
C
C-----------------------------------------------------------------------
C---- PROCEDURE (CG - Complex Greek)
30018 LOCUC = 2027
      LOCLC = 2127
      LOC0 = 2200
      ASSIGN 20034 TO NPR015
      GO TO 30015
20034 ASSIGN 20035 TO NPR016
      GO TO 30016
20035 ASSIGN 20036 TO NPR019
      GO TO 30019
20036 GO TO 20216
C
C-----------------------------------------------------------------------
C---- PROCEDURE (CI - Complex Italic)
30020 LOCUC = 2051
      LOCLC = 2151
      LOC0 = 2750
      ASSIGN 20037 TO NPR015
      GO TO 30015
20037 ASSIGN 20038 TO NPR016
      GO TO 30016
20038 ASSIGN 20039 TO NPR007
      GO TO 30007
20039 GO TO 20215
C
C-----------------------------------------------------------------------
C---- PROCEDURE (Complex Script Special Characters)
30021 LOC0SV = LOC0
      LOC0 = 2200
      ASSIGN 20040 TO NPR015
      GO TO 30015
20040 LOC0 = LOC0SV
      ASCII(SPACE -31,NFONT) = LOC0 - 1
      ASCII(EXCLPT-31,NFONT) = LOC0 + 14
      ASCII(DQUOTE-31,NFONT) = LOC0 + 28
      ASCII(AMPSND-31,NFONT) = LOC0 + 18
      ASCII(SQUOTE-31,NFONT) = LOC0 + 27
      ASCII(COLON -31,NFONT) = LOC0 + 12
      ASCII(SCOLON-31,NFONT) = LOC0 + 13
      ASCII(QUERY -31,NFONT) = LOC0 + 15
      GO TO 20056
C
C-----------------------------------------------------------------------
C---- PROCEDURE (Complex Special Characters)
30015 NOCHAR = LOC0 - 1
      ASSIGN 20041 TO NPR011
      GO TO 30011
20041 ASCII(SPACE -31,NFONT) = LOC0 - 1
      ASCII(EXCLPT-31,NFONT) = LOC0 + 14
      ASCII(DQUOTE-31,NFONT) = LOC0 + 17
      ASCII(NUMBER-31,NFONT) = LOC0 + 75
      ASCII(PERCNT-31,NFONT) = LOC0 + 71
      ASCII(AMPSND-31,NFONT) = LOC0 + 72
      ASCII(SQUOTE-31,NFONT) = LOC0 + 16
      ASCII(COLON -31,NFONT) = LOC0 + 12
      ASCII(SCOLON-31,NFONT) = LOC0 + 13
      ASCII(LANGLE-31,NFONT) = LOC0 + 41
      ASCII(RANGLE-31,NFONT) = LOC0 + 42
      ASCII(QUERY -31,NFONT) = LOC0 + 15
      ASCII(LBRAKT-31,NFONT) = LOC0 + 23
      ASCII(RBRAKT-31,NFONT) = LOC0 + 24
      ASCII(CARET -31,NFONT) = LOC0 + 47
      ASCII(ACCENT-31,NFONT) = LOC0 + 49
      ASCII(LBRACE-31,NFONT) = LOC0 + 25
      ASCII(VBAR  -31,NFONT) = LOC0 + 29
      ASCII(RBRACE-31,NFONT) = LOC0 + 26
      ASCII(TILDE -31,NFONT) = LOC0 + 46
      GO TO NPR015, (20031,20034,20037,20040,20053,20078,20095,20132,202
     X56)
C
C-----------------------------------------------------------------------
C---- PROCEDURE (Complex Standard Signs)
30016 I = (FVAR-1)*2 + FCASE
      NX0050=I
      IF (NX0050.LT.1.OR.NX0050.GT.8) GO TO 20050
      GO TO (20042,20043,20044,20045,20046,20047,20048,20049), NX0050
20042 ASCII(COMMA -31,NFONT) = 2211
      ASCII(PERIOD-31,NFONT) = 2210
      ASCII(LPAREN-31,NFONT) = 2221
      ASCII(RPAREN-31,NFONT) = 2222
      ASCII(MINUS -31,NFONT) = 2231
      ASCII(PLUS  -31,NFONT) = 2232
      ASCII(STAR  -31,NFONT) = 2219
      ASCII(SLASH -31,NFONT) = 2220
      ASCII(EQUALS-31,NFONT) = 2238
      ASCII(DOLLAR-31,NFONT) = 2274
      ASCII(AT    -31,NFONT) = 2273
      GO TO 20051
20043 ASCII(COMMA -31,NFONT) = 2211
      ASCII(PERIOD-31,NFONT) = 2210
      ASCII(LPAREN-31,NFONT) = 2221
      ASCII(RPAREN-31,NFONT) = 2222
      ASCII(MINUS -31,NFONT) = 2231
      ASCII(PLUS  -31,NFONT) = 2232
      ASCII(STAR  -31,NFONT) = 2219
      ASCII(SLASH -31,NFONT) = 2220
      ASCII(EQUALS-31,NFONT) = 2238
      ASCII(DOLLAR-31,NFONT) = 2274
      ASCII(AT    -31,NFONT) = 2273
      GO TO 20051
20044 ASCII(COMMA -31,NFONT) = 2213
      ASCII(PERIOD-31,NFONT) = 2215
      ASCII(LPAREN-31,NFONT) = 2405
      ASCII(RPAREN-31,NFONT) = 2406
      ASCII(MINUS -31,NFONT) = 2256
      ASCII(PLUS  -31,NFONT) = 2257
      ASCII(STAR  -31,NFONT) = 2259
      ASCII(SLASH -31,NFONT) = 2258
      ASCII(EQUALS-31,NFONT) = 2260
      ASCII(DOLLAR-31,NFONT) = 2279
      ASCII(AT    -31,NFONT) = 2276
      GO TO 20051
20045 ASCII(COMMA -31,NFONT) = 2213
      ASCII(PERIOD-31,NFONT) = 2215
      ASCII(LPAREN-31,NFONT) = 2223
      ASCII(RPAREN-31,NFONT) = 2224
      ASCII(MINUS -31,NFONT) = 2246
      ASCII(PLUS  -31,NFONT) = 2272
      ASCII(STAR  -31,NFONT) = 2245
      ASCII(SLASH -31,NFONT) = 2271
      ASCII(EQUALS-31,NFONT) = 2239
      ASCII(DOLLAR-31,NFONT) = 2275
      ASCII(AT    -31,NFONT) = 2216
      GO TO 20051
20046 ASCII(COMMA -31,NFONT) = 2212
      ASCII(PERIOD-31,NFONT) = 2214
      ASCII(LPAREN-31,NFONT) = 2403
      ASCII(RPAREN-31,NFONT) = 2404
      ASCII(MINUS -31,NFONT) = 2231
      ASCII(PLUS  -31,NFONT) = 2232
      ASCII(STAR  -31,NFONT) = 2235
      ASCII(SLASH -31,NFONT) = 2230
      ASCII(EQUALS-31,NFONT) = 2238
      ASCII(DOLLAR-31,NFONT) = 2411
      ASCII(AT    -31,NFONT) = 2277
      GO TO 20051
20047 ASCII(COMMA -31,NFONT) = 2212
      ASCII(PERIOD-31,NFONT) = 2214
      ASCII(LPAREN-31,NFONT) = 2221
      ASCII(RPAREN-31,NFONT) = 2222
      ASCII(MINUS -31,NFONT) = 2231
      ASCII(PLUS  -31,NFONT) = 2232
      ASCII(STAR  -31,NFONT) = 2236
      ASCII(SLASH -31,NFONT) = 2229
      ASCII(EQUALS-31,NFONT) = 2238
      ASCII(DOLLAR-31,NFONT) = 2267
      ASCII(AT    -31,NFONT) = 2217
      GO TO 20051
20048 ASCII(COMMA -31,NFONT) = 2251
      ASCII(PERIOD-31,NFONT) = 2252
      ASCII(LPAREN-31,NFONT) = 2407
      ASCII(RPAREN-31,NFONT) = 2408
      ASCII(MINUS -31,NFONT) = 2261
      ASCII(PLUS  -31,NFONT) = 2233
      ASCII(STAR  -31,NFONT) = 2242
      ASCII(SLASH -31,NFONT) = 2228
      ASCII(EQUALS-31,NFONT) = 2244
      ASCII(DOLLAR-31,NFONT) = 2412
      ASCII(AT    -31,NFONT) = 2270
      GO TO 20051
20049 ASCII(COMMA -31,NFONT) = 2251
      ASCII(PERIOD-31,NFONT) = 2252
      ASCII(LPAREN-31,NFONT) = 2225
      ASCII(RPAREN-31,NFONT) = 2226
      ASCII(MINUS -31,NFONT) = 2263
      ASCII(PLUS  -31,NFONT) = 2234
      ASCII(STAR  -31,NFONT) = 2241
      ASCII(SLASH -31,NFONT) = 2227
      ASCII(EQUALS-31,NFONT) = 2243
      ASCII(DOLLAR-31,NFONT) = 2268
      ASCII(AT    -31,NFONT) = 2218
      GO TO 20051
20050 ASSIGN 20052 TO NPR013
      GO TO 30013
20052 CONTINUE
20051 GO TO NPR016, (20032,20035,20038,20054,20057,20076,20254)
C
C-----------------------------------------------------------------------
C---- PROCEDURE (CR - Complex Roman)
30022 LOCUC = 2001
      LOCLC = 2101
      LOC0 = 2200
      ASSIGN 20053 TO NPR015
      GO TO 30015
20053 ASSIGN 20054 TO NPR016
      GO TO 30016
20054 ASSIGN 20055 TO NPR007
      GO TO 30007
20055 GO TO 20214
C
C-----------------------------------------------------------------------
C---- PROCEDURE (CS - Complex Script)
30023 LOCUC = 2551
      LOCLC = 2651
      LOC0 = 2750
      ASSIGN 20056 TO NPR021
      GO TO 30021
20056 ASSIGN 20057 TO NPR016
      GO TO 30016
20057 ASSIGN 20058 TO NPR007
      GO TO 30007
20058 GO TO 20217
C
C-----------------------------------------------------------------------
C---- PROCEDURE (Cyrillic Alphanumerics)
30017 IF (FCASE .EQ. 2) LOCUC = LOCLC
      NX0061=FVAR
      IF (NX0061.LT.1.OR.NX0061.GT.2) GO TO 20061
      GO TO (20059,20060), NX0061
20059 ASCII(UCA-31,NFONT) = LOCUC + CYA
      ASCII(UCB-31,NFONT) = LOCUC + CYB
      ASCII(UCV-31,NFONT) = LOCUC + CYV
      ASCII(UCG-31,NFONT) = LOCUC + CYG
      ASCII(UCD-31,NFONT) = LOCUC + CYD
      ASCII(UCE-31,NFONT) = LOCUC + CYYE
      ASCII(UCC-31,NFONT) = LOCUC + CYZHE
      ASCII(UCZ-31,NFONT) = LOCUC + CYZ
      ASCII(UCI-31,NFONT) = LOCUC + CYEE
      ASCII(UCK-31,NFONT) = LOCUC + CYK
      ASCII(UCL-31,NFONT) = LOCUC + CYL
      ASCII(UCM-31,NFONT) = LOCUC + CYM
      ASCII(UCN-31,NFONT) = LOCUC + CYN
      ASCII(UCO-31,NFONT) = LOCUC + CYO
      ASCII(UCP-31,NFONT) = LOCUC + CYP
      ASCII(UCR-31,NFONT) = LOCUC + CYR
      ASCII(UCS-31,NFONT) = LOCUC + CYS
      ASCII(UCT-31,NFONT) = LOCUC + CYT
      ASCII(UCU-31,NFONT) = LOCUC + CYOO
      ASCII(UCF-31,NFONT) = LOCUC + CYF
      ASCII(UCX-31,NFONT) = LOCUC + CYKHA
      ASCII(UCH-31,NFONT) = LOCUC + CYTSE
      ASCII(UCJ-31,NFONT) = LOCUC + CYCHE
      ASCII(UCQ-31,NFONT) = LOCUC + CYSH
      ASCII(UCW-31,NFONT) = LOCUC + CYSHCH
      ASCII(UCY-31,NFONT) = LOCUC + CYYIRI
      ASCII(LBRAKT-31,NFONT) = LOCUC + CYYOO
      ASCII(RSLANT-31,NFONT) = LOCUC + CYYA
      ASCII(RBRAKT-31,NFONT) = LOCUC + CYMZNK
      ASCII(STAR  -31,NFONT) = LOCUC + CYMZNK
      GO TO 20062
20060 ASCII(UCA-31,NFONT) = LOCUC + CYA
      ASCII(UCB-31,NFONT) = LOCUC + CYB
      ASCII(UCV-31,NFONT) = LOCUC + CYV
      ASCII(UCG-31,NFONT) = LOCUC + CYG
      ASCII(UCD-31,NFONT) = LOCUC + CYD
      ASCII(UCE-31,NFONT) = LOCUC + CYE
      ASCII(UCC-31,NFONT) = LOCUC + CYZ
      ASCII(UCZ-31,NFONT) = LOCUC + CYZ
      ASCII(UCI-31,NFONT) = LOCUC + CYEEK
      ASCII(UCK-31,NFONT) = LOCUC + CYK
      ASCII(UCL-31,NFONT) = LOCUC + CYL
      ASCII(UCM-31,NFONT) = LOCUC + CYM
      ASCII(UCN-31,NFONT) = LOCUC + CYN
      ASCII(UCO-31,NFONT) = LOCUC + CYO
      ASCII(UCP-31,NFONT) = LOCUC + CYP
      ASCII(UCR-31,NFONT) = LOCUC + CYR
      ASCII(UCS-31,NFONT) = LOCUC + CYS
      ASCII(UCT-31,NFONT) = LOCUC + CYT
      ASCII(UCU-31,NFONT) = LOCUC + CYOO
      ASCII(UCF-31,NFONT) = LOCUC + CYF
      ASCII(UCX-31,NFONT) = LOCUC + CYK
      ASCII(UCH-31,NFONT) = NOCHAR
      ASCII(UCJ-31,NFONT) = LOCUC + CYCHE
      ASCII(UCQ-31,NFONT) = LOCUC + CYSH
      ASCII(UCW-31,NFONT) = NOCHAR
      ASCII(UCY-31,NFONT) = LOCUC + CYYIRI
      ASCII(LBRAKT-31,NFONT) = NOCHAR
      ASCII(RSLANT-31,NFONT) = NOCHAR
      ASCII(RBRAKT-31,NFONT) = LOCUC + CYTZNK
      ASCII(STAR  -31,NFONT) = LOCUC + CYTZNK
20061 CONTINUE
20062 I =1
      GO TO 20064
20063 I =I +1
20064 IF ((29-I ).LT.0) GO TO 20065
      IUC = I + 64 - 31
      ILC = IUC + 32
      IF (.NOT.(ASCII(IUC,NFONT) .NE. NOCHAR)) GO TO 20067
      ASCII(ILC,NFONT) = ASCII(IUC,NFONT) + LOCLC - LOCUC
      GO TO 20068
20067 ASCII(ILC,NFONT) = NOCHAR
20068 GO TO 20063
20065 ASSIGN 20070 TO NPR008
      GO TO 30008
20070 GO TO 20033
C
C-----------------------------------------------------------------------
C---- PROCEDURE (Digits)
30008 I =0
      GO TO 20072
20071 I =I +1
20072 IF ((9-I ).LT.0) GO TO 20073
      IDIG = I + 48 - 31
      ASCII(IDIG,NFONT) = LOC0 + I
      GO TO 20071
20073 GO TO NPR008, (20011,20070,20109)
C
C-----------------------------------------------------------------------
C---- PROCEDURE (DR - Duplex Roman)
30024 LOCUC = 2501
      LOCLC = 2601
      LOC0 = 2700
      ASSIGN 20075 TO NPR025
      GO TO 30025
20075 ASSIGN 20076 TO NPR016
      GO TO 30016
20076 ASSIGN 20077 TO NPR007
      GO TO 30007
20077 GO TO 20218
C
C-----------------------------------------------------------------------
C---- PROCEDURE (Duplex Special Characters)
30025 LOC0SV = LOC0
      LOC0 = 2200
      ASSIGN 20078 TO NPR015
      GO TO 30015
20078 LOC0 = LOC0SV
      ASCII(SPACE -31,NFONT) = LOC0 - 1
      ASCII(EXCLPT-31,NFONT) = LOC0 + 14
      ASCII(DQUOTE-31,NFONT) = LOC0 + 28
      ASCII(AMPSND-31,NFONT) = LOC0 + 18
      ASCII(SQUOTE-31,NFONT) = LOC0 + 27
      ASCII(COLON -31,NFONT) = LOC0 + 12
      ASCII(SCOLON-31,NFONT) = LOC0 + 13
      ASCII(QUERY -31,NFONT) = LOC0 + 15
      GO TO 20075
C
C-----------------------------------------------------------------------
C---- PROCEDURE (Fill Font with Symbol for Unsupported Character)
30011 I =32
      GO TO 20080
20079 I =I +1
20080 IF ((127-I ).LT.0) GO TO 20081
      ASCII(I-31,NFONT) = NOCHAR
      GO TO 20079
20081 GO TO NPR011, (20023,20041)
C
C-----------------------------------------------------------------------
C---- PROCEDURE (GE - Gothic English)
30026 FVAR = MIN0(FVAR,2)
      LOCUC = 3501
      LOCLC = 3601
      LOC0 = 3700
      ASSIGN 20083 TO NPR027
      GO TO 30027
20083 ASSIGN 20084 TO NPR028
      GO TO 30028
20084 ASSIGN 20085 TO NPR007
      GO TO 30007
20085 GO TO 20220
C
C-----------------------------------------------------------------------
C---- PROCEDURE (GG - Gothic German)
30029 FVAR = MIN0(FVAR,2)
      LOCUC = 3301
      LOCLC = 3401
      LOC0 = 3700
      ASSIGN 20086 TO NPR027
      GO TO 30027
20086 ASSIGN 20087 TO NPR028
      GO TO 30028
20087 ASSIGN 20088 TO NPR007
      GO TO 30007
20088 IF (.NOT.(FVAR .EQ. 2)) GO TO 20089
      I = UCS + 32
      ASCII(I-31,NFONT) = LOCLC + 26
20089 ASCII(LBRAKT-31,NFONT) = LOCUC + 29
      ASCII(RSLANT-31,NFONT) = LOCUC + 30
      ASCII(RBRAKT-31,NFONT) = LOCUC + 31
      ASCII(LBRACE-31,NFONT) = LOCLC + 29
      ASCII(VBAR  -31,NFONT) = LOCLC + 30
      ASCII(RBRACE-31,NFONT) = LOCLC + 31
      GO TO 20222
C
C-----------------------------------------------------------------------
C---- PROCEDURE (GI - Gothic Italian)
30030 FVAR = MIN0(FVAR,2)
      LOCUC = 3801
      LOCLC = 3901
      LOC0 = 3700
      ASSIGN 20092 TO NPR027
      GO TO 30027
20092 ASSIGN 20093 TO NPR028
      GO TO 30028
20093 ASSIGN 20094 TO NPR007
      GO TO 30007
20094 GO TO 20221
C
C-----------------------------------------------------------------------
C---- PROCEDURE (Gothic Special Characters)
30027 LOC0SV = LOC0
      LOC0 = 2200
      ASSIGN 20095 TO NPR015
      GO TO 30015
20095 LOC0 = LOC0SV
      ASCII(SPACE -31,NFONT) = LOC0 - 1
      ASCII(EXCLPT-31,NFONT) = LOC0 + 14
      ASCII(DQUOTE-31,NFONT) = LOC0 + 28
      ASCII(AMPSND-31,NFONT) = LOC0 + 18
      ASCII(SQUOTE-31,NFONT) = LOC0 + 27
      ASCII(COLON -31,NFONT) = LOC0 + 12
      ASCII(SCOLON-31,NFONT) = LOC0 + 13
      ASCII(QUERY -31,NFONT) = LOC0 + 15
      GO TO NPR027, (20083,20086,20092)
C
C-----------------------------------------------------------------------
C---- PROCEDURE (Gothic Standard Signs)
30028 ASCII(COMMA -31,NFONT) = 3711
      ASCII(PERIOD-31,NFONT) = 3710
      ASCII(LPAREN-31,NFONT) = 3721
      ASCII(RPAREN-31,NFONT) = 3722
      ASCII(MINUS -31,NFONT) = 3724
      ASCII(PLUS  -31,NFONT) = 3725
      ASCII(STAR  -31,NFONT) = 3723
      ASCII(SLASH -31,NFONT) = 3720
      ASCII(EQUALS-31,NFONT) = 3726
      ASCII(DOLLAR-31,NFONT) = 3719
      ASCII(AT    -31,NFONT) = 2273
      GO TO NPR028, (20084,20087,20093)
C
C-----------------------------------------------------------------------
C---- PROCEDURE (Greek Alphanumerics)
30019 IF (FCASE .EQ. 2) LOCUC = LOCLC
      NX0100=FVAR
      IF (NX0100.LT.1.OR.NX0100.GT.4) GO TO 20100
      GO TO (20096,20097,20098,20099), NX0100
20096 ASCII(UCA-31,NFONT) = LOCUC + ALPHA
      ASCII(UCB-31,NFONT) = LOCUC + BETA
      ASCII(UCG-31,NFONT) = LOCUC + GAMMA
      ASCII(UCD-31,NFONT) = LOCUC + DELTA
      ASCII(UCE-31,NFONT) = LOCUC + EPSLON
      ASCII(UCZ-31,NFONT) = LOCUC + ZETA
      ASCII(UCQ-31,NFONT) = LOCUC + THETA
      ASCII(UCI-31,NFONT) = LOCUC + IOTA
      ASCII(UCK-31,NFONT) = LOCUC + KAPPA
      ASCII(UCL-31,NFONT) = LOCUC + LAMBDA
      ASCII(UCM-31,NFONT) = LOCUC + MU
      ASCII(UCN-31,NFONT) = LOCUC + NU
      ASCII(UCX-31,NFONT) = LOCUC + XI
      ASCII(UCO-31,NFONT) = LOCUC + OMCRON
      ASCII(UCP-31,NFONT) = LOCUC + PI
      ASCII(UCR-31,NFONT) = LOCUC + RHO
      ASCII(UCS-31,NFONT) = LOCUC + SIGMA
      ASCII(UCT-31,NFONT) = LOCUC + TAU
      ASCII(UCU-31,NFONT) = LOCUC + UPSLON
      ASCII(UCY-31,NFONT) = LOCUC + UPSLON
      ASCII(UCF-31,NFONT) = LOCUC + PHI
      ASCII(UCC-31,NFONT) = LOCUC + CHI
      ASCII(UCW-31,NFONT) = LOCUC + PSI
      ASCII(UCH-31,NFONT) = NOCHAR
      ASCII(UCJ-31,NFONT) = NOCHAR
      ASCII(UCV-31,NFONT) = NOCHAR
      GO TO 20101
20097 ASCII(UCA-31,NFONT) = LOCUC + ALPHA
      ASCII(UCB-31,NFONT) = LOCUC + BETA
      ASCII(UCG-31,NFONT) = LOCUC + GAMMA
      ASCII(UCD-31,NFONT) = LOCUC + DELTA
      ASCII(UCE-31,NFONT) = LOCUC + ETA
      ASCII(UCZ-31,NFONT) = LOCUC + ZETA
      ASCII(UCQ-31,NFONT) = LOCUC + TAU
      ASCII(UCI-31,NFONT) = LOCUC + IOTA
      ASCII(UCK-31,NFONT) = LOCUC + KAPPA
      ASCII(UCL-31,NFONT) = LOCUC + LAMBDA
      ASCII(UCM-31,NFONT) = LOCUC + MU
      ASCII(UCN-31,NFONT) = LOCUC + NU
      ASCII(UCX-31,NFONT) = LOCUC + XI
      ASCII(UCO-31,NFONT) = LOCUC + OMEGA
      ASCII(UCP-31,NFONT) = LOCUC + PI
      ASCII(UCR-31,NFONT) = LOCUC + RHO
      ASCII(UCS-31,NFONT) = LOCUC + SIGMA
      ASCII(UCT-31,NFONT) = LOCUC + TAU
      ASCII(UCU-31,NFONT) = LOCUC + UPSLON
      ASCII(UCY-31,NFONT) = LOCUC + UPSLON
      ASCII(UCF-31,NFONT) = LOCUC + PI
      ASCII(UCC-31,NFONT) = LOCUC + CHI
      ASCII(UCW-31,NFONT) = NOCHAR
      ASCII(UCH-31,NFONT) = NOCHAR
      ASCII(UCJ-31,NFONT) = NOCHAR
      ASCII(UCV-31,NFONT) = NOCHAR
      GO TO 20101
20098 ASCII(UCA-31,NFONT) = LOCUC + ALPHA
      ASCII(UCB-31,NFONT) = LOCUC + BETA
      ASCII(UCG-31,NFONT) = LOCUC + GAMMA
      ASCII(UCD-31,NFONT) = LOCUC + DELTA
      ASCII(UCE-31,NFONT) = LOCUC + EPSLON
      ASCII(UCZ-31,NFONT) = LOCUC + ZETA
      ASCII(UCH-31,NFONT) = LOCUC + ETA
      ASCII(UCQ-31,NFONT) = LOCUC + THETA
      ASCII(UCI-31,NFONT) = LOCUC + IOTA
      ASCII(UCK-31,NFONT) = LOCUC + KAPPA
      ASCII(UCL-31,NFONT) = LOCUC + LAMBDA
      ASCII(UCM-31,NFONT) = LOCUC + MU
      ASCII(UCN-31,NFONT) = LOCUC + NU
      ASCII(UCX-31,NFONT) = LOCUC + XI
      ASCII(UCO-31,NFONT) = LOCUC + OMCRON
      ASCII(UCP-31,NFONT) = LOCUC + PI
      ASCII(UCR-31,NFONT) = LOCUC + RHO
      ASCII(UCS-31,NFONT) = LOCUC + SIGMA
      ASCII(UCT-31,NFONT) = LOCUC + TAU
      ASCII(UCU-31,NFONT) = LOCUC + UPSLON
      ASCII(UCF-31,NFONT) = LOCUC + PHI
      ASCII(UCC-31,NFONT) = LOCUC + CHI
      ASCII(UCY-31,NFONT) = LOCUC + PSI
      ASCII(UCW-31,NFONT) = LOCUC + OMEGA
      ASCII(UCJ-31,NFONT) = NOCHAR
      ASCII(UCV-31,NFONT) = NOCHAR
      GO TO 20101
20099 ASCII(UCA-31,NFONT) = LOCUC + ALPHA
      ASCII(UCB-31,NFONT) = LOCUC + BETA
      ASCII(UCG-31,NFONT) = LOCUC + GAMMA
      ASCII(UCD-31,NFONT) = LOCUC + DELTA
      ASCII(UCE-31,NFONT) = LOCUC + EPSLON
      ASCII(UCZ-31,NFONT) = LOCUC + ZETA
      ASCII(UCH-31,NFONT) = LOCUC + ETA
      ASCII(UCQ-31,NFONT) = LOCUC + THETA
      ASCII(UCI-31,NFONT) = LOCUC + IOTA
      ASCII(UCK-31,NFONT) = LOCUC + KAPPA
      ASCII(UCL-31,NFONT) = LOCUC + LAMBDA
      ASCII(UCM-31,NFONT) = LOCUC + MU
      ASCII(UCN-31,NFONT) = LOCUC + NU
      ASCII(UCX-31,NFONT) = LOCUC + XI
      ASCII(UCO-31,NFONT) = LOCUC + OMCRON
      ASCII(UCP-31,NFONT) = LOCUC + PI
      ASCII(UCR-31,NFONT) = LOCUC + RHO
      ASCII(UCS-31,NFONT) = LOCUC + SIGMA
      ASCII(UCT-31,NFONT) = LOCUC + TAU
      ASCII(UCU-31,NFONT) = LOCUC + UPSLON
      ASCII(UCF-31,NFONT) = LOCUC + PHI
      ASCII(UCC-31,NFONT) = LOCUC + CHI
      ASCII(UCY-31,NFONT) = LOCUC + PSI
      ASCII(UCW-31,NFONT) = LOCUC + OMEGA
      ASCII(UCJ-31,NFONT) = NOCHAR
      ASCII(UCV-31,NFONT) = NOCHAR
20100 CONTINUE
20101 I =1
      GO TO 20103
20102 I =I +1
20103 IF ((26-I ).LT.0) GO TO 20104
      IUC = I + 64 - 31
      ILC = IUC + 32
      IF (.NOT.(ASCII(IUC,NFONT) .NE. NOCHAR)) GO TO 20106
      ASCII(ILC,NFONT) = ASCII(IUC,NFONT) + LOCLC - LOCUC
      GO TO 20107
20106 ASCII(ILC,NFONT) = NOCHAR
20107 GO TO 20102
20104 ASSIGN 20109 TO NPR008
      GO TO 30008
20109 NX0114=FVAR
      IF (NX0114.LT.1.OR.NX0114.GT.4) GO TO 20114
      GO TO (20110,20111,20112,20113), NX0114
20110 GO TO 20115
20111 IF (.NOT.(FTYPE .NE. KG)) GO TO 20116
      ASCII(LCS-31,NFONT) = LOCLC + 60
20116 GO TO 20115
20112 GO TO 20115
20113 IF (.NOT.(FTYPE .NE. KG)) GO TO 20119
      ASCII(LCE-31,NFONT) = LOCLC + 57
      ASCII(LCQ-31,NFONT) = LOCLC + 58
      ASCII(LCF-31,NFONT) = LOCLC + 59
      IF (.NOT.(FTYPE .EQ. SG)) GO TO 20122
      ASCII(LCD-31,NFONT) = LOCLC + 56
      GO TO 20123
20122 ASCII(LCD-31,NFONT) = LOCLC + 138
20123 CONTINUE
20119 GO TO 20115
20114 ASSIGN 20125 TO NPR013
      GO TO 30013
20125 CONTINUE
20115 GO TO NPR019, (20036,20128,20165,20238)
C
C-----------------------------------------------------------------------
C---- PROCEDURE (IG - Indexical Greek)
30031 LOCUC = 1027
      LOCLC = 1127
      LOC0 = 1200
      ASSIGN 20126 TO NPR032
      GO TO 30032
20126 ASSIGN 20127 TO NPR033
      GO TO 30033
20127 ASSIGN 20128 TO NPR019
      GO TO 30019
20128 GO TO 20210
C
C-----------------------------------------------------------------------
C---- PROCEDURE (II - Indexical Italic)
30034 LOCUC = 1051
      LOCLC = 1151
      LOC0 = 2750
      ASSIGN 20129 TO NPR032
      GO TO 30032
20129 ASSIGN 20130 TO NPR033
      GO TO 30033
20130 ASSIGN 20131 TO NPR007
      GO TO 30007
20131 GO TO 20209
C
C-----------------------------------------------------------------------
C---- PROCEDURE (Indexical Special Characters)
30032 ASSIGN 20132 TO NPR015
      GO TO 30015
20132 GO TO NPR032, (20126,20129,20151)
C
C-----------------------------------------------------------------------
C---- PROCEDURE (Indexical Standard Signs)
30033 I = (FVAR-1)*2 + FCASE
      NX0141=I
      IF (NX0141.LT.1.OR.NX0141.GT.8) GO TO 20141
      GO TO (20133,20134,20135,20136,20137,20138,20139,20140), NX0141
20133 ASCII(COMMA -31,NFONT) = 1211
      ASCII(PERIOD-31,NFONT) = 1210
      ASCII(LPAREN-31,NFONT) = 1221
      ASCII(RPAREN-31,NFONT) = 1222
      ASCII(MINUS -31,NFONT) = 1231
      ASCII(PLUS  -31,NFONT) = 1232
      ASCII(STAR  -31,NFONT) = 1219
      ASCII(SLASH -31,NFONT) = 1220
      ASCII(EQUALS-31,NFONT) = 1238
      ASCII(DOLLAR-31,NFONT) = 1274
      ASCII(AT    -31,NFONT) = 1273
      GO TO 20142
20134 ASCII(COMMA -31,NFONT) = 1211
      ASCII(PERIOD-31,NFONT) = 1210
      ASCII(LPAREN-31,NFONT) = 1221
      ASCII(RPAREN-31,NFONT) = 1222
      ASCII(MINUS -31,NFONT) = 1231
      ASCII(PLUS  -31,NFONT) = 1232
      ASCII(STAR  -31,NFONT) = 1219
      ASCII(SLASH -31,NFONT) = 1220
      ASCII(EQUALS-31,NFONT) = 1238
      ASCII(DOLLAR-31,NFONT) = 1274
      ASCII(AT    -31,NFONT) = 1273
      GO TO 20142
20135 ASCII(COMMA -31,NFONT) = 1213
      ASCII(PERIOD-31,NFONT) = 1215
      ASCII(LPAREN-31,NFONT) = 1405
      ASCII(RPAREN-31,NFONT) = 1406
      ASCII(MINUS -31,NFONT) = 1256
      ASCII(PLUS  -31,NFONT) = 1257
      ASCII(STAR  -31,NFONT) = 1259
      ASCII(SLASH -31,NFONT) = 1258
      ASCII(EQUALS-31,NFONT) = 1260
      ASCII(DOLLAR-31,NFONT) = 1279
      ASCII(AT    -31,NFONT) = 1276
      GO TO 20142
20136 ASCII(COMMA -31,NFONT) = 1213
      ASCII(PERIOD-31,NFONT) = 1215
      ASCII(LPAREN-31,NFONT) = 1223
      ASCII(RPAREN-31,NFONT) = 1224
      ASCII(MINUS -31,NFONT) = 1246
      ASCII(PLUS  -31,NFONT) = 1272
      ASCII(STAR  -31,NFONT) = 1245
      ASCII(SLASH -31,NFONT) = 1271
      ASCII(EQUALS-31,NFONT) = 1239
      ASCII(DOLLAR-31,NFONT) = 1275
      ASCII(AT    -31,NFONT) = 1216
      GO TO 20142
20137 ASCII(COMMA -31,NFONT) = 1212
      ASCII(PERIOD-31,NFONT) = 1214
      ASCII(LPAREN-31,NFONT) = 1403
      ASCII(RPAREN-31,NFONT) = 1404
      ASCII(MINUS -31,NFONT) = 1231
      ASCII(PLUS  -31,NFONT) = 1232
      ASCII(STAR  -31,NFONT) = 1235
      ASCII(SLASH -31,NFONT) = 1230
      ASCII(EQUALS-31,NFONT) = 1238
      ASCII(DOLLAR-31,NFONT) = 1411
      ASCII(AT    -31,NFONT) = 1277
      GO TO 20142
20138 ASCII(COMMA -31,NFONT) = 1212
      ASCII(PERIOD-31,NFONT) = 1214
      ASCII(LPAREN-31,NFONT) = 1221
      ASCII(RPAREN-31,NFONT) = 1222
      ASCII(MINUS -31,NFONT) = 1231
      ASCII(PLUS  -31,NFONT) = 1232
      ASCII(STAR  -31,NFONT) = 1236
      ASCII(SLASH -31,NFONT) = 1229
      ASCII(EQUALS-31,NFONT) = 1238
      ASCII(DOLLAR-31,NFONT) = 1267
      ASCII(AT    -31,NFONT) = 1217
      GO TO 20142
20139 ASCII(COMMA -31,NFONT) = 1251
      ASCII(PERIOD-31,NFONT) = 1252
      ASCII(LPAREN-31,NFONT) = 1407
      ASCII(RPAREN-31,NFONT) = 1408
      ASCII(MINUS -31,NFONT) = 1261
      ASCII(PLUS  -31,NFONT) = 1233
      ASCII(STAR  -31,NFONT) = 1242
      ASCII(SLASH -31,NFONT) = 1228
      ASCII(EQUALS-31,NFONT) = 1244
      ASCII(DOLLAR-31,NFONT) = 1412
      ASCII(AT    -31,NFONT) = 1270
      GO TO 20142
20140 ASCII(COMMA -31,NFONT) = 1251
      ASCII(PERIOD-31,NFONT) = 1252
      ASCII(LPAREN-31,NFONT) = 1225
      ASCII(RPAREN-31,NFONT) = 1226
      ASCII(MINUS -31,NFONT) = 1263
      ASCII(PLUS  -31,NFONT) = 1234
      ASCII(STAR  -31,NFONT) = 1241
      ASCII(SLASH -31,NFONT) = 1227
      ASCII(EQUALS-31,NFONT) = 1243
      ASCII(DOLLAR-31,NFONT) = 1268
      ASCII(AT    -31,NFONT) = 1218
      GO TO 20142
20141 ASSIGN 20143 TO NPR013
      GO TO 30013
20143 CONTINUE
20142 GO TO NPR033, (20127,20130,20152)
C
C-----------------------------------------------------------------------
C---- PROCEDURE (Initialize for New Font)
30001 IF (.NOT.(NFUSED .LE. 0)) GO TO 20144
      I =1
      N20147=MAXFNT
      GO TO 20148
20147 I =I +1
20148 IF ((N20147-I ).LT.0) GO TO 20149
      FSWTCH(I) = NUL
      USWTCH(I) = NUL
      LSWTCH(I) = NUL
      GO TO 20147
20149 NFUSED = 0
20144 ERROR = .FALSE.
      NFONT = MOD(NFONT,MAXFNT) + 1
      KFONT = NFONT
      NFUSED = MIN0(NFUSED+1,MAXFNT)
      FSWTCH(NFONT) = KARASC(SWCHAR(1))
      IF (KARCM2(SWCHAR,1,1H ,1,1) .EQ. 0) FSWTCH(NFONT) = NUL
      USWTCH(NFONT) = LANGLE
      LSWTCH(NFONT) = RANGLE
      BSWTCH(NFONT) = NUL
      GO TO 20001
C
C-----------------------------------------------------------------------
C---- PROCEDURE (Internal Error)
30013 CALL ERRMS (6HSYMSF ,12,14HInternal error,14)
      CALL ERRCK
      ERROR = .TRUE.
      IF (ERROR) STOP
      GO TO NPR013, (20030,20052,20125,20143,20226,20246)
C
C-----------------------------------------------------------------------
C---- PROCEDURE (IR - Indexical Roman)
30035 LOCUC = 1001
      LOCLC = 1101
      LOC0 = 1200
      ASSIGN 20151 TO NPR032
      GO TO 30032
20151 ASSIGN 20152 TO NPR033
      GO TO 30033
20152 ASSIGN 20153 TO NPR007
      GO TO 30007
20153 GO TO 20208
C
C-----------------------------------------------------------------------
C---- PROCEDURE (Issue Error Messages and Supply Defaults)
30005 IF (.NOT.(FCASE .EQ. 0)) GO TO 20154
      CALL ERRMS (6HSYMSF ,8,
     X38HInvalid font case - Upper-Case assumed,38)
      CALL ERRAT (6HFONT  ,1,FONT,5)
      FCASE = 1
20154 IF (.NOT.(FTYPE .EQ. 0)) GO TO 20157
      CALL ERRMS (6HSYMSF ,8,
     X41HInvalid font type - Triplex Roman assumed,41)
      CALL ERRAT (6HFONT  ,1,FONT,5)
      FCASE = TR
20157 IF (.NOT.(FVAR .EQ. 0)) GO TO 20160
      CALL ERRMS (6HSYMSF ,8,
     X32HInvalid font variant - 1 assumed,32)
      CALL ERRAT (6HFONT  ,1,FONT,5)
      FVAR = 1
20160 CALL ERRCK
      GO TO 20005
C
C-----------------------------------------------------------------------
C---- PROCEDURE (KG - Cartographic Greek)
30036 LOCUC = 27
      LOCLC = 27
      LOC0 = 200
      ASSIGN 20163 TO NPR010
      GO TO 30010
20163 ASSIGN 20164 TO NPR012
      GO TO 30012
20164 ASSIGN 20165 TO NPR019
      GO TO 30019
20165 GO TO 20207
C
C-----------------------------------------------------------------------
C---- PROCEDURE (KR - Cartographic Roman)
30037 LOCUC = 1
      LOCLC = 1
      LOC0 = 200
      ASSIGN 20166 TO NPR010
      GO TO 30010
20166 ASSIGN 20167 TO NPR012
      GO TO 30012
20167 ASSIGN 20168 TO NPR007
      GO TO 30007
20168 GO TO 20206
C
C-----------------------------------------------------------------------
C---- PROCEDURE (SA - Simplex ASCII)
30038 LOCUC = 1565
      LOCLC = 1597
      I =32
      GO TO 20170
20169 I =I +1
20170 IF ((127-I ).LT.0) GO TO 20171
      ASCII(I-31,NFONT) = 1500 + I
      GO TO 20169
20171 IF (.NOT.(FCASE .EQ. 2)) GO TO 20173
      I =65
      GO TO 20177
20176 I =I +1
20177 IF ((90-I ).LT.0) GO TO 20178
      ASCII(I-31,NFONT) = 1500 + I + 32
      GO TO 20176
20178 CONTINUE
20173 GO TO 20224
C
C-----------------------------------------------------------------------
C---- PROCEDURE (Set Complete Font Specifications)
30006 FONTID(NFONT) = (FCASE-1)*256 + (FTYPE-1)*8 + FVAR - 1
      CALL KARUPK (FONTNM(1,NFONT),FONT,1,5)
      I =1
      GO TO 20181
20180 I =I +1
20181 IF ((5-I ).LT.0) GO TO 20182
      FONTNM(I,NFONT) = KARUC(FONTNM(I,NFONT))
      GO TO 20180
20182 NX0204=FTYPE
      IF (NX0204.LT.1.OR.NX0204.GT.20) GO TO 20204
      GO TO (20184,20185,20186,20187,20188,20189,20190,20191,20192,20193
     X,20194,20195,20196,20197,20198,20199,20200,20201,20202,20203), NX0
     X204
20184 ASSIGN 20206 TO NPR037
      GO TO 30037
20206 GO TO 20205
20185 ASSIGN 20207 TO NPR036
      GO TO 30036
20207 GO TO 20205
20186 ASSIGN 20208 TO NPR035
      GO TO 30035
20208 GO TO 20205
20187 ASSIGN 20209 TO NPR034
      GO TO 30034
20209 GO TO 20205
20188 ASSIGN 20210 TO NPR031
      GO TO 30031
20210 GO TO 20205
20189 ASSIGN 20211 TO NPR039
      GO TO 30039
20211 GO TO 20205
20190 ASSIGN 20212 TO NPR040
      GO TO 30040
20212 GO TO 20205
20191 ASSIGN 20213 TO NPR041
      GO TO 30041
20213 GO TO 20205
20192 ASSIGN 20214 TO NPR022
      GO TO 30022
20214 GO TO 20205
20193 ASSIGN 20215 TO NPR020
      GO TO 30020
20215 GO TO 20205
20194 ASSIGN 20216 TO NPR018
      GO TO 30018
20216 GO TO 20205
20195 ASSIGN 20217 TO NPR023
      GO TO 30023
20217 GO TO 20205
20196 ASSIGN 20218 TO NPR024
      GO TO 30024
20218 GO TO 20205
20197 ASSIGN 20219 TO NPR042
      GO TO 30042
20219 GO TO 20205
20198 ASSIGN 20220 TO NPR026
      GO TO 30026
20220 GO TO 20205
20199 ASSIGN 20221 TO NPR030
      GO TO 30030
20221 GO TO 20205
20200 ASSIGN 20222 TO NPR029
      GO TO 30029
20222 GO TO 20205
20201 ASSIGN 20223 TO NPR014
      GO TO 30014
20223 GO TO 20205
20202 ASSIGN 20224 TO NPR038
      GO TO 30038
20224 GO TO 20205
20203 ASSIGN 20225 TO NPR009
      GO TO 30009
20225 GO TO 20205
20204 ASSIGN 20226 TO NPR013
      GO TO 30013
20226 CONTINUE
20205 GO TO 20006
C
C-----------------------------------------------------------------------
C---- PROCEDURE (Set Font Case)
30002 IF (.NOT.(KARCM2(FONT,1,2HUC,1,2) .EQ. 0)) GO TO 20227
      FCASE = 1
      GO TO 20228
20227 IF (.NOT.(KARCM2(FONT,1,2HLC,1,2) .EQ. 0)) GO TO 10001
      FCASE = 2
      GO TO 20228
10001 FCASE = 0
      ERROR = .TRUE.
20228 GO TO 20002
C
C-----------------------------------------------------------------------
C---- PROCEDURE (Set Font Type)
30003 IF (.NOT.(KARCM2(FONT,3,2HKR,1,2) .EQ. 0)) GO TO 20230
      FTYPE = KR
      GO TO 20231
20230 IF (.NOT.(KARCM2(FONT,3,2HKG,1,2) .EQ. 0)) GO TO 10002
      FTYPE = KG
      GO TO 20231
10002 IF (.NOT.(KARCM2(FONT,3,2HIR,1,2) .EQ. 0)) GO TO 10003
      FTYPE = IR
      GO TO 20231
10003 IF (.NOT.(KARCM2(FONT,3,2HII,1,2) .EQ. 0)) GO TO 10004
      FTYPE = II
      GO TO 20231
10004 IF (.NOT.(KARCM2(FONT,3,2HIG,1,2) .EQ. 0)) GO TO 10005
      FTYPE = IG
      GO TO 20231
10005 IF (.NOT.(KARCM2(FONT,3,2HSR,1,2) .EQ. 0)) GO TO 10006
      FTYPE = SR
      GO TO 20231
10006 IF (.NOT.(KARCM2(FONT,3,2HSS,1,2) .EQ. 0)) GO TO 10007
      FTYPE = SS
      GO TO 20231
10007 IF (.NOT.(KARCM2(FONT,3,2HSG,1,2) .EQ. 0)) GO TO 10008
      FTYPE = SG
      GO TO 20231
10008 IF (.NOT.(KARCM2(FONT,3,2HCR,1,2) .EQ. 0)) GO TO 10009
      FTYPE = CR
      GO TO 20231
10009 IF (.NOT.(KARCM2(FONT,3,2HCI,1,2) .EQ. 0)) GO TO 10010
      FTYPE = CI
      GO TO 20231
10010 IF (.NOT.(KARCM2(FONT,3,2HCG,1,2) .EQ. 0)) GO TO 10011
      FTYPE = CG
      GO TO 20231
10011 IF (.NOT.(KARCM2(FONT,3,2HCS,1,2) .EQ. 0)) GO TO 10012
      FTYPE = CS
      GO TO 20231
10012 IF (.NOT.(KARCM2(FONT,3,2HDR,1,2) .EQ. 0)) GO TO 10013
      FTYPE = DR
      GO TO 20231
10013 IF (.NOT.(KARCM2(FONT,3,2HTR,1,2) .EQ. 0)) GO TO 10014
      FTYPE = TR
      GO TO 20231
10014 IF (.NOT.(KARCM2(FONT,3,2HGE,1,2) .EQ. 0)) GO TO 10015
      FTYPE = GE
      GO TO 20231
10015 IF (.NOT.(KARCM2(FONT,3,2HGI,1,2) .EQ. 0)) GO TO 10016
      FTYPE = GI
      GO TO 20231
10016 IF (.NOT.(KARCM2(FONT,3,2HGG,1,2) .EQ. 0)) GO TO 10017
      FTYPE = GG
      GO TO 20231
10017 IF (.NOT.(KARCM2(FONT,3,2HCC,1,2) .EQ. 0)) GO TO 10018
      FTYPE = CC
      GO TO 20231
10018 IF (.NOT.(KARCM2(FONT,3,2HSA,1,2) .EQ. 0)) GO TO 10019
      FTYPE = SA
      GO TO 20231
10019 IF (.NOT.(KARCM2(FONT,3,2HBA,1,2) .EQ. 0)) GO TO 10020
      FTYPE = BA
      GO TO 20231
10020 FTYPE = 0
      ERROR = .TRUE.
20231 GO TO 20003
C
C-----------------------------------------------------------------------
C---- PROCEDURE (Set Font Variant)
30004 IF (.NOT.(KARCM2(FONT,5,1H1,1,1) .EQ. 0)) GO TO 20233
      FVAR = 1
      GO TO 20234
20233 IF (.NOT.(KARCM2(FONT,5,1H2,1,1) .EQ. 0)) GO TO 10021
      FVAR = 2
      GO TO 20234
10021 IF (.NOT.(KARCM2(FONT,5,1H3,1,1) .EQ. 0)) GO TO 10022
      FVAR = 3
      GO TO 20234
10022 IF (.NOT.(KARCM2(FONT,5,1H4,1,1) .EQ. 0)) GO TO 10023
      FVAR = 4
      GO TO 20234
10023 FVAR = 0
      ERROR = .TRUE.
20234 GO TO 20004
C
C-----------------------------------------------------------------------
C---- PROCEDURE (SG - Simplex Greek)
30041 LOCUC = 527
      LOCLC = 627
      LOC0 = 700
      ASSIGN 20236 TO NPR043
      GO TO 30043
20236 ASSIGN 20237 TO NPR044
      GO TO 30044
20237 ASSIGN 20238 TO NPR019
      GO TO 30019
20238 GO TO 20213
C
C-----------------------------------------------------------------------
C---- PROCEDURE (Simplex Special Characters)
30043 ASSIGN 20239 TO NPR010
      GO TO 30010
20239 GO TO NPR043, (20236,20247,20250)
C
C-----------------------------------------------------------------------
C---- PROCEDURE (Simplex Standard Signs)
30044 NX0244=FVAR
      IF (NX0244.LT.1.OR.NX0244.GT.4) GO TO 20244
      GO TO (20240,20241,20242,20243), NX0244
20240 ASCII(COMMA -31,NFONT) =  711
      ASCII(PERIOD-31,NFONT) =  710
      ASCII(LPAREN-31,NFONT) =  721
      ASCII(RPAREN-31,NFONT) =  722
      ASCII(MINUS -31,NFONT) =  724
      ASCII(PLUS  -31,NFONT) =  725
      ASCII(STAR  -31,NFONT) =  728
      ASCII(SLASH -31,NFONT) =  720
      ASCII(EQUALS-31,NFONT) =  726
      ASCII(DOLLAR-31,NFONT) =  719
      ASCII(AT    -31,NFONT) = 1273
      GO TO 20245
20241 ASCII(COMMA -31,NFONT) =  713
      ASCII(PERIOD-31,NFONT) =  715
      ASCII(LPAREN-31,NFONT) =  721
      ASCII(RPAREN-31,NFONT) =  722
      ASCII(MINUS -31,NFONT) =  724
      ASCII(PLUS  -31,NFONT) =  725
      ASCII(STAR  -31,NFONT) =  729
      ASCII(SLASH -31,NFONT) =  720
      ASCII(EQUALS-31,NFONT) =  726
      ASCII(DOLLAR-31,NFONT) =  733
      ASCII(AT    -31,NFONT) =  732
      GO TO 20245
20242 ASCII(COMMA -31,NFONT) =  712
      ASCII(PERIOD-31,NFONT) =  714
      ASCII(LPAREN-31,NFONT) =  721
      ASCII(RPAREN-31,NFONT) =  722
      ASCII(MINUS -31,NFONT) =  724
      ASCII(PLUS  -31,NFONT) =  725
      ASCII(STAR  -31,NFONT) =  727
      ASCII(SLASH -31,NFONT) =  720
      ASCII(EQUALS-31,NFONT) =  726
      ASCII(DOLLAR-31,NFONT) =  723
      ASCII(AT    -31,NFONT) =  730
      GO TO 20245
20243 ASCII(COMMA -31,NFONT) =  716
      ASCII(PERIOD-31,NFONT) =  717
      ASCII(LPAREN-31,NFONT) =  721
      ASCII(RPAREN-31,NFONT) =  722
      ASCII(MINUS -31,NFONT) =  724
      ASCII(PLUS  -31,NFONT) =  725
      ASCII(STAR  -31,NFONT) =  718
      ASCII(SLASH -31,NFONT) =  720
      ASCII(EQUALS-31,NFONT) =  726
      ASCII(DOLLAR-31,NFONT) =  735
      ASCII(AT    -31,NFONT) =  731
      GO TO 20245
20244 ASSIGN 20246 TO NPR013
      GO TO 30013
20246 CONTINUE
20245 GO TO NPR044, (20237,20248,20251)
C
C-----------------------------------------------------------------------
C---- PROCEDURE (SR - Simplex Roman)
30039 LOCUC = 501
      LOCLC = 601
      LOC0 = 700
      ASSIGN 20247 TO NPR043
      GO TO 30043
20247 ASSIGN 20248 TO NPR044
      GO TO 30044
20248 ASSIGN 20249 TO NPR007
      GO TO 30007
20249 GO TO 20211
C
C-----------------------------------------------------------------------
C---- PROCEDURE (SS - Simplex Script)
30040 LOCUC = 551
      LOCLC = 651
      LOC0 = 700
      ASSIGN 20250 TO NPR043
      GO TO 30043
20250 ASSIGN 20251 TO NPR044
      GO TO 30044
20251 ASSIGN 20252 TO NPR007
      GO TO 30007
20252 GO TO 20212
C
C-----------------------------------------------------------------------
C---- PROCEDURE (TR - Triplex Roman)
30042 LOCUC = 3001
      LOCLC = 3101
      LOC0 = 3200
      ASSIGN 20253 TO NPR045
      GO TO 30045
20253 ASSIGN 20254 TO NPR016
      GO TO 30016
20254 ASSIGN 20255 TO NPR007
      GO TO 30007
20255 GO TO 20219
C
C-----------------------------------------------------------------------
C---- PROCEDURE (Triplex Special Characters)
30045 LOC0SV = LOC0
      LOC0 = 2200
      ASSIGN 20256 TO NPR015
      GO TO 30015
20256 LOC0 = LOC0SV
      ASCII(SPACE -31,NFONT) = LOC0 - 1
      ASCII(EXCLPT-31,NFONT) = LOC0 + 14
      ASCII(DQUOTE-31,NFONT) = LOC0 + 28
      ASCII(AMPSND-31,NFONT) = LOC0 + 18
      ASCII(SQUOTE-31,NFONT) = LOC0 + 27
      ASCII(COLON -31,NFONT) = LOC0 + 12
      ASCII(SCOLON-31,NFONT) = LOC0 + 13
      ASCII(QUERY -31,NFONT) = LOC0 + 15
      GO TO 20253
C
      END
