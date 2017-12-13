# ===========
# = Day 4   =
# ===========
eval(parse("inputs/input4.txt"))
wl <- strsplit(input4, "\\n")[[1]]

# Part 1
sum(sapply(wl, function(x) { 
  y <- strsplit(x, " ")[[1]]
  length(y) == length(unique(y))
}))

# Part 2
sum(sapply(wl, function(x) { 
  y <- strsplit(x, " ")[[1]]
  l <- length(y)
  if (l != length(unique(y))) return(0)
  for (i in 1:(l - 1)) {
    ci <- strsplit(y[i], "")[[1]]
    for (j in (i + 1):l) {
      cj <- strsplit(y[j], "")[[1]]
      if (length(ci) == length(cj) && all(sort(ci) == sort(cj))) return(0)
    }
  }
  return(1)
}))
