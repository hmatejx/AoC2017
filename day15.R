# ===========
# = Day 15  =
# ===========

input15 <- c(277, 349)
state <- input15

generate1 <- function() {
  state <<- (state * c(16807, 48271)) %% 2147483647
  return(state)
}

generate2 <- function() {
  # A
  repeat {
    state[1] <<- (state[1] * 16807) %% 2147483647
    if (state[1] %% 4 == 0) break
  }
  # B
  repeat {
    state[2] <<- (state[2] * 48271) %% 2147483647
    if (state[2] %% 8 == 0) break
  }
  return(state)
}

# Part 1
total1 <- 0
N1 <- 40000000
for (i in 1:N1) {
  if (i %% (N1 / 1000) == 0) 
    cat("\r", format(i / N1 * 100, digits = 4, width = 5), "% ...")

  generate1()

  b1 <- rev(intToBits(state[1]))
  b2 <- rev(intToBits(state[2]))
  total1 <- total1 + all(b1[17:32] == b2[17:32])
}
cat(total1)

# Part 2
total2 <- 0
N2 <- 5000000
for (i in 1:N2) {
  if (i %% (N2 / 1000) == 0) 
    cat("\r", format(i / N2 * 100, digits = 4, width = 5), "% ...")
  
  generate2()
  
  b1 <- rev(intToBits(state[1]))
  b2 <- rev(intToBits(state[2]))
  total2 <- total2 + all(b1[17:32] == b2[17:32])
}
cat(total2)
