
      SubRoutine CLS
*
*     Clears the screen.
*     Put machine dependent code here.
*
        Character Esc,Clear_String*6

        Esc=CHAR(27)
        Clear_String(1:1)=Esc
        Clear_String(2:2)='['
        Clear_String(3:3)='H'
        Clear_String(4:4)=Esc
        Clear_String(5:5)='['
        Clear_String(6:6)='J'
        Write(5,10)Clear_String
10      Format(1x,6a)
      End
