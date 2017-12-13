# ===========
# = Day 5   =
# ===========

eval(parse("input5.txt"))

# Part 1
jmps <- input5
i <- 1
j <- 0
while (i >= 1 && i <= length(jmps)) {
  newi <- i + jmps[i]
  jmps[i] <- jmps[i] + 1
  i <- newi
  j <- j + 1
}
print(j)

# Part 2
jmps <- input5
i <- 1
j <- 0
while (i >= 1 && i <= length(jmps)) {
  newi <- i + jmps[i]
  if (jmps[i] >= 3) {
    jmps[i] <- jmps[i] - 1
  } else {
    jmps[i] <- jmps[i] + 1
  }
  i <- newi
  j <- j + 1
}
print(j)
