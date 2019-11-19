# gawk program for parsing undeformed/deformed geometry data files.

    BEGIN {p = 0}

# first pass through file.
(pass == 1) {
    if (p == 1) {
      if ($1 == "1") {
        printf "\"Undeformed\n+linestyle(dashed)\n"
        previous_segment = $6
      }
      if (previous_segment != $6) {
        printf "\n+linestyle(dashed)\n"
        previous_segment = $6
      }
      print $2, $3
    }
    if ($0 == "*") {
      getline
      p = 1
    }
}

# second pass through file.
(pass == 2) {
    if (p == 1) {
      if ($1 == "1") {
        printf "\n\"Deformed\n"
        previous_segment = $6
      }
      if (previous_segment != $6) {
        printf "\n"
        previous_segment = $6
      }
      print $4, $5
    }
    if ($0 == "*") {
      getline
      p = 1
    }
}

# end
