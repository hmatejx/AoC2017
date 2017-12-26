# ===========
# = Day 21  =
# ===========

input21 <- lapply(strsplit(readChar("inputs/input21.txt", file.size("inputs/input21.txt")), "\r\n")[[1]],
                  function(line) { strsplit(gsub(" => ", " ", line), " ")[[1]] })
input21 <- lapply(input21, function(rule) {
  tmp1 <- strsplit(strsplit(rule[1], "/", fixed = T)[[1]], "")
  tmp2 <- strsplit(strsplit(rule[2], "/", fixed = T)[[1]], "")
  i <- matrix(unlist(tmp1), nrow = length(tmp1[[1]]), byrow = T)
  o <- matrix(unlist(tmp2), nrow = length(tmp2[[1]]), byrow = T)
  list(dim = nrow(i), input = i, output = o)
})

# matrix rotation and flip operations
flip_v <- function(m) { apply(m, 2, rev) }
flip_h <- function(m) { apply(m, 1, rev) }
rot_cw <- function(m) { t(apply(m, 2, rev)) }
rot_ccw <- function(m) { apply(t(m), 2, rev) }

# initial image
image <- matrix(c(".", "#", ".",
                  ".", ".", "#",
                  "#", "#", "#"), nrow = 3, byrow = T)

library(memoise)
match <- function(part) {
  # find the matching rule (can be optimised)
  rule <- 0
  for (i in 1:length(input21)) {
    r <- input21[[i]]
    if (r$dim != nrow(part))
      next
    p = r$input
    p.flip <- flip_v(p)
    if (all(part == p) || all(part == rot_cw(p)) ||
        all(part == rot_cw(rot_cw(p))) || all(part == rot_ccw(p)) ||
        all(part == p.flip) || all(part == rot_cw(p.flip)) ||
        all(part == rot_cw(rot_cw(p.flip))) || all(part == rot_ccw(p.flip))) {
      rule <- i
      break
    }
  }
  input21[[rule]]$output
}
match <- memoise(match)

iteration <- function() {
  # get rule size
  dim <- nrow(image)
  isize <- c(3, 2)[(dim %% 2 == 0) + 1]
  div <- dim / isize
  osize <- ifelse(isize == 2, 3, 4)
  output <- matrix("", div*osize, div*osize)
  # replace all parts
  for (i in 1:div)
    for (j in 1:div)
      output[((i - 1)*osize + 1):(i*osize), ((j - 1)*osize + 1):(j*osize)] <-
        match(image[((i - 1)*isize + 1):(i*isize), ((j - 1)*isize + 1):(j*isize)])
  image <<- output
}

# Part 1
for (i in 1:5) { iteration() }
cat(sum((image == "#")), "\n")

# Part 2
for (i in 1:13) {
  cat("iteration", i + 5, "\timage size =", nrow(image), "...\n")
  iteration()
}
cat(sum((image == "#")), "\n")
