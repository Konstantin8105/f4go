        program main
            integer i
            i = 12
            i = i + 12
            i = i - 20
            i = i *2
            write (*, FMT = 120) i
            STOp
*
  120 Format (' output ', I1 , ' integer')
        END
