# ===========
# = Day 21  =
# ===========

input21 <- lapply(strsplit(readChar("input21.txt", file.size("input21.txt")), "\r\n")[[1]],
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

# extend pattern list
patterns <- list()
j <- 1
for (i in 1:length(input21)) {
  p1 <- input21[[i]]$input; p2 <- rot_cw(p1); p3 <- rot_cw(rot_cw(p1)); p4 <- rot_ccw(p1)
  p5 <- flip_v(p1); p6 <- rot_cw(p5); p7 <- rot_cw(rot_cw(p5)); p8 <- rot_ccw(p5)
  patterns[[j]] <- list(dim = input21[[i]]$dim, input = p1, output = input21[[i]]$output); j <- j + 1
  if (all(p2 == p1)) next
  patterns[[j]] <- list(dim = input21[[i]]$dim, input = p2, output = input21[[i]]$output); j <- j + 1
  if (all(p3 == p1) || all(p3 == p2)) next
  patterns[[j]] <- list(dim = input21[[i]]$dim, input = p3, output = input21[[i]]$output); j <- j + 1
  if (all(p4 == p1) || all(p4 == p2) || all(p4 == p3)) next
  patterns[[j]] <- list(dim = input21[[i]]$dim, input = p4, output = input21[[i]]$output); j <- j + 1
  if (all(p5 == p1) || all(p5 == p2) || all(p5 == p3) || all(p5 == p4)) next
  patterns[[j]] <- list(dim = input21[[i]]$dim, input = p5, output = input21[[i]]$output); j <- j + 1
  if (all(p6 == p1) || all(p6 == p2) || all(p6 == p3) || all(p6 == p4) || all(p6 == p5)) next
  patterns[[j]] <- list(dim = input21[[i]]$dim, input = p6, output = input21[[i]]$output); j <- j + 1
  if (all(p7 == p1) || all(p7 == p2) || all(p7 == p3) || all(p7 == p4) || all(p7 == p5) || all(p7 == p6)) next
  patterns[[j]] <- list(dim = input21[[i]]$dim, input = p7, output = input21[[i]]$output); j <- j + 1
  if (all(p8 == p1) || all(p8 == p2) || all(p8 == p3) || all(p8 == p4) || all(p8 == p5) || all(p8 == p6) || all(p8 == p7)) next
  patterns[[j]] <- list(dim = input21[[i]]$dim, input = p8, output = input21[[i]]$output); j <- j + 1
}


# initial image
image <- matrix(c(".", "#", ".",
                  ".", ".", "#",
                  "#", "#", "#"), nrow = 3, byrow = T)

match <- function(part) {
  # find the matching rule (can be optimised)
  rule <- 0
  for (i in 1:length(patterns)) {
    r <- patterns[[i]]
    if (r$dim != nrow(part))
      next
    if (all(part == r$input)) {
      rule <- i
      break
    }
  }
  patterns[[rule]]$output
}

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
