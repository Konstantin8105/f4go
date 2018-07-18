       PROGRAM cows
          INTEGER :: n, n3
          INTEGER :: n2
          n = 306
          n2 = n + 2 * n / 4 
          n2 = n2 + n * 2
          n2 = n2 + n - 2
          n2 = n2 + n ** (n + 1) + n ** (n2 - n)
          n2 = (n2 + n / 2) * ( n2 + n ) + 42
          n2 = 2 * (n2 + n / 2) - 42
          n3 = n + n2
       END PROGRAM
