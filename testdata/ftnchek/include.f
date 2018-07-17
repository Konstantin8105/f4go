C  Derived from Brian Downing's WC program, replacing common decls by INCLUDEs
C
C     main(){   Get a file, open it, read and determine semi-useful
C               statistics, print them to screen, and exit quietly.
C           };
C
C     This program is an example word counter that makes use of several
C     Fortran intrinsic functions and data structures, such as; 
C     common, sub-routines, functions, inplied do loops, and much, much more.
C
      Program WC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Program:    Word_Count_And_Other_Stuff                                 C
C     Written_By: Brian Downing                                              C
C                 Fordham University                                         C
C     Date:       October 1st-16th, 1990                                     C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      Character Fname*80

      Call Initialize
      Call GetFileName(Fname)
      Call GetStats(Fname)
      Call PrintStats
      End
C
C     SubRoutine to get all kinds of neat statistics.
C
      SubRoutine GetStats(Fname)
      Include 'stats.h'
      Character Inline*82, Fname*80, Ch

      Open (Unit=8,File=Fname,Err=999)
      Do While (.TRUE.)
          Read(8,10,End=888)InLine
          NL = NL + 1
          LastPos = INDEX(InLine,'  ')
          Do J = 1,LastPos
              Ch = InLine(J:J)
              L = IntUpCase(ICHAR(Ch))
              NEC(L) = NEC(L) + 1
              If ((Ch.NE.' ').AND.(Ch.NE.'.')) Then
                  NC = NC + 1
              ElseIf (Ch.EQ.'.') Then            
                  NP = NP + 1
              Else
                  NW = NW + 1
              EndIf
          EndDo
      EndDo
888   Continue
      ACPW = REAL(NC)/REAL(NW)
      AWPS = REAL(NW)/REAL(NP)
      Return
10    Format(a)
999   Print*,'Error opening file, please verify filename and try again.'
C
C     In the event of improper filename exit abruptly.
C
      STOP
      End
C
C     SubRoutine to print to terminal all of these neat statistics.
C
      SubRoutine PrintStats
      Include 'stats.h'

      Write(5,10)ACPW,AWPS,NW,NP,NL,NC
      Write(5,20)
      Do J = 65,90
          Write(5,40)(CHAR(J),NEC(J),('@',K=1,(NEC(J)/10)),
     1           ('*',K=1,MOD(NEC(J),10)))
      EndDo
      Write(5,50)                      
10    Format('1'30X'Word Statistics'/1x,80('*')/
     1       1X'Average characters per word = 'F6.2/
     2       1X'Average words per sentence  = 'F6.2/
     3       1X'Total number of words       = 'I5/
     4       1X'Total number of sentences   = 'I5/
     5       1X'Total number of lines       = 'I5/
     6       1X'Total number of characters  = 'I5/)
20    Format(29x'Character Statistics'/1x,80('*')/)
30    Format(1X,A)
40    Format(1X,A','I3,1x,125(A))
50    Format(1X'Legend:'/9x'@ equals ten characters',
     1         ', * equals one character.')
      Return      
      End
C
C     SubRoutine to prompt for and return a filename.
C
      SubRoutine GetFileName(Fname)
      Character Fname*80, Prompt*7

      Prompt = '_File: '
      Write(5,10)Prompt
      Read(5,20)Fname
10    Format(1XA$)
20    Format(A)
      Return
      End
C
C     SubRoutine to initailize globally used variables.
C
      SubRoutine Initialize
      Common /Stats/A,B,J,K,L,M,N(26)
      Do O = 1,26
        N(O) = 0
      EndDo
      A = 0.0
      B = 0.0
      J = 0
      K = 0
      L = 0
      M = 0
      Return
      End
C
C     Function to return integer value of a character in range of uppercase.
C
      Function IntUpCase (I)

      If ((I.LE.ICHAR('z')).AND.(I.GE.ICHAR('a'))) Then
          IntUpCase = I - ICHAR(' ')
      Else
          IntUpCase = I
      EndIf
      Return
      End
