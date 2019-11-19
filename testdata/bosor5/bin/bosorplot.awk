# bosorplot.awk splits the {case}.PLT2 file into separate plot files.

BEGIN {
   split(ARGV[1], a, ".")
}

{ 
  if ($1 == "$END") {
    close("temp")
    system("/bin/mv temp " name)
    next
  }

  if ($1 == "*") {
    name = fixname(prev)
    name = a[1] ".." name
  }

  prev = $0
  print $0 > "temp"

} # MAIN BODY

function fixname(s) {
  while (index(s, "= "))
    sub(/\= /, "=", s)
  end
  while (index(s, "  "))
    sub(/  /, " ", s)
  end
  gsub(/ /, "_", s)
  return s
}
