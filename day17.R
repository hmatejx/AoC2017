# ===========
# = Day 17  =
# ===========

input17 <- 337

# initial state
buffer <- c(0)
pos <- 0
round <- 1

# version modifying the buffer
spin <- function(step) {
  l <- length(buffer)
  pos <<- (pos + step) %% l + 1
  res <- c(buffer[1:pos], round)
  if (pos + 1 <= l) 
    res <- c(res, buffer[(pos + 1):l])
  buffer <<- res
  round <<- round + 1
  invisible()
}

# version not modifying the buffer
spin2 <- function(step) {
  pos <- 0
  res <- 0
  for (i in 1:50000001) {
    pos <- (pos + step) %% i + 1
    if (pos == 1)
      final <- i
  }
  invisible(final)
}

# day 1
for (i in 1:2017) spin(input17)
cat(buffer[pos + 2])

# day 2
cat(spin2(input17))
