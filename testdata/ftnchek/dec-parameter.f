C      IMPLICIT INTEGER(a-z)

      PARAMETER shipmoves=3
      PARAMETER fightermoves=8
      PARAMETER (stdparam = 7)
      INTEGER g2(100)
      INTEGER hits(15),tipe(15),crahit(15),craloc(15)

      BYTE      specal,pass
      BYTE      ab,ac,ad,ao,e,own1,own2,own      !AVOID WORD REFERENCES TO THESE
      INTEGER rlmap(3000)
      LOGICAL      automv,autosave,savedone
 
      COMMON/AB9/ab9,prior,nshprf
      COMMON/ARMTOT/armtot
      end
