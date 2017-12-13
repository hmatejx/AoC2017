# ===========
# = Day 6   =
# ===========

input6 <- c(4, 1, 15, 12, 0, 9, 9, 5, 5, 8, 7, 3, 14, 5, 12, 3)

# Part 1
banks <- input6
l <- length(banks)
tostr <- function(x) { paste0(x, collapse = "|") }
states <- list(tostr(banks))
repeat {
  # find largest bank
  i <- which.max(banks)
  # set that bank to 0 and redistribute
  b <- banks[i]
  banks[i] <- 0
  for (j in 1:b) {
    k <- (i + j - 1) %% l + 1
    banks[k] <- banks[k] + 1
  }
  key <- tostr(banks)
  # compare with previous states and break if repeating
  if (key %in% states)
    break
  # add to states
  states <- c(states, key)
}
length(states)

# Part 2
banks <- input6
l <- length(banks)
tostr <- function(x) { paste0(x, collapse = "|") }
lastkey <- ""
states <- list(tostr(banks))
repeat {
  # find largest bank
  i <- which.max(banks)
  # set that bank to 0 and redistribute
  b <- banks[i]
  banks[i] <- 0
  for (j in 1:b) {
    k <- (i + j - 1) %% l + 1
    banks[k] <- banks[k] + 1
  }
  key <- tostr(banks)
  # compare with previous states and break if repeating
  if (key %in% states) {
    lastkey <<- key
    break
  }
  # add to states
  states <- c(states, key)
}
length(states) - which(states == lastkey) + 1
