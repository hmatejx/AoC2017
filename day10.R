# ===========
# = Day 10  =
# ===========

input10 <- list(len = as.numeric(read.csv("inputs/input10.txt", header = F)),
                str = gsub("\r\n", "", readChar("inputs/input10.txt", file.size("inputs/input10.txt")), fixed = T))

round <- function(input, state = list(string = 0:255, skip = 0, pos = 0)) {
  within(state,
    for (len in input) {
      idx <- (pos:(pos + len - 1)) %% 256 + 1
      string[idx] <- rev(string[idx])
      pos <- (pos + len + skip - 1) %% 256 + 1
      skip <- (skip + 1) %% 256
    }
  )
}

hash <- function(inputstr) {
  inputlen <- c(as.numeric(charToRaw(inputstr)), 17, 31, 73, 47, 23)
  rnd <- list(string = 0:255, skip = 0, pos = 0)
  for (i in 1:64) rnd <- round(inputlen, rnd)
  return(sparse2dense(rnd$string))
}

sparse2dense <- function(string) {
  res <- ""
  for (i in 1:16) {
    sparse <- as.raw(string[(16*(i - 1) + 1):(16*i)])
    dense <- sparse[1]
    for (j in 2:16) dense <- xor(dense, sparse[j])
    res <- paste0(res, as.character(dense))
  }
  return(res)
}

# part 1
prod(round(input10$len)$string[1:2])

# part 2
hash(input10$str)
