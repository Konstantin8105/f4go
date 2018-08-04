        program main
            integer i
            i = 12
            i = i + 12
            write (*, FMT = 120) i
            STOp
*
  120 Format (' output ', I2 , ' integer')
        END
