# ===========
# = Day 16  =
# ===========

input16 <- strsplit(readChar("input16.txt", file.size("input16.txt")), ",", fixed = T)[[1]]
moves <- input16

# initial state
prgs <- letters[1:16]

dance <- function(move) {
  type <- substr(move, 1, 1)
  par <- substring(move, 2)
  if (type == "s") {
    s <- as.numeric(par)
    prgs[(s:(15 + s) %% 16) + 1] <<- prgs
  } else if (type == "x") {
    pair <- as.numeric(strsplit(par, "/")[[1]]) + 1
    prgs[pair] <<- prgs[pair[2:1]]
  } else if (type == "p") {
    pair <- which(prgs %in% (strsplit(par, "/")[[1]]))
    prgs[pair] <<- prgs[pair[2:1]]
  }
}

complete_dance <- function(dummy) { sapply(moves, dance); invisible() }

# Day 1
complete_dance()
cat(paste0(prgs, collapse = ""))

# Day 2
# find cycle length
prgs <- letters[1:16]
cycle <- 1
repeat {
  complete_dance()
  if (all(prgs == letters[1:16]))
    break
  cycle <- cycle + 1
}
# dance the remainder
prgs <- letters[1:16]
rem <- 1000000000 %% cycle
invisible(sapply(1:rem, complete_dance))
cat(paste0(prgs, collapse = ""))
