      character*100 c
      c = 'line\
       break'
      c = 'LINE\
     $ BREAK'
      c = '\142\040\40\132_\x5ag'
      c = 'abcdefg'
      c = "abcdefg"
      c = c// "don't"
      c = 'He said "yes," I think'
      c = "\n, \\"
      c = "\""
      c = '\' !'
C the next one tests unix_backslash
      c = '\'!'
      end
