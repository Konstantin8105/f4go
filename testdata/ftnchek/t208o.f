*     Parameters for which statement order must be preserved
      real a, b, c
      parameter (c = 1.23)
      parameter (b = 2.0*c)
      parameter (a = (b + c))

*     Parameter with continuation lines
      real d
      parameter (d
     x
     x  =
     x
     x  4.0)

*     Parameter with continuation lines separated by comment lines

      parameter (e
*     comment line
*     another comment line
     x     =
*     yet another comment line
*     still another comment line
     x     5
     x     .
     x     4
     x   
     x     )

*     Parameter with embedded comment
      real t
      parameter (t = ! this is a comment
     x        25.0)

*     Parameters with quoted strings.
      integer u
      parameter (u = b'0110011010011001')

      integer v
      parameter (v = z'ffff')

      integer w
      parameter (w = 'ffff'x)

      integer x
      parameter (x = x'ffff')

      character*(*) y
      parameter (y = 'yes, isn''t this nice')

      character*(*) z
      parameter (z = 'O''Neil''s brother''s pub')

*     Parameters with long text past column 72
      character*(*) s
      parameter (s = '123456789.123456789.123456789.123456789.123456789.SEQ#0001
     x123456789.123456789.123456789.123456789.123456789.123456789.123456SEQ#0002
     x789.123456789.123456789.123456789.123456789.123456789.123456789.12SEQ#0003
     x3456789.123456789.123456789.123456789.123456789.123456789.12345678SEQ#0004
     x9.123456789.123456789.123456789.123456789.123456789.')            SEQ#0005
      end
