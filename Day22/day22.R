# ===========
# = Day 22  =
# ===========

library(collections)
library(progress)

# initial grid
input22 <- t(sapply(strsplit(readChar("input22.txt", file.size("input22.txt")), "\r\n", fixed = T)[[1]],
                    function(line) { strsplit(line, "", fixed = T)[[1]] }, USE.NAMES = F))
grid <- dict()
grid2 <- dict()
for (i in 1:nrow(input22)) {
  for (j in 1:ncol(input22)) {
    grid$set(c(j, i), input22[i, j])
    grid2$set(c(j, i), input22[i, j])
  }
}

draw <- function(grid) {
  xr <- range(sapply(grid$keys(), function(p) { p[1] }))
  yr <- range(sapply(grid$keys(), function(p) { p[2] }))
  for (i in yr[1]:yr[2]) {
    s <- ""
    for (j in xr[1]:xr[2]) {
      s <- paste0(s, " ", grid$get(c(j, i), '.')) 
    }
    cat(s, "\n")
  }
}

# Part 1
# initial state
pos <- c(as.integer((ncol(input22) + 1) %/% 2), as.integer((nrow(input22) + 1) %/% 2))
direction <- c(0L, -1L)
nbursts <- 10000
ninfections <- 0
for (b in 1:nbursts) {
  node <- grid$get(pos, '.')
  # new direction
  if (node == '#'  ) direction <- c(-direction[2], direction[1])
  else               direction <- c(direction[2], -direction[1])
  # new node state
  if (node == '.') { grid$set(pos, '#'); ninfections <- ninfections + 1 }
  else               grid$set(pos, '.')
  pos <- pos + direction
}
#draw(grid)
cat("Part 1:", ninfections, "\n")

# Part 2
pos <- c(as.integer((ncol(input22) + 1) %/% 2), as.integer((nrow(input22) + 1) %/% 2))
direction <- c(0L, -1L)
nbursts <- 10000000
pb <- progress_bar$new(total = nbursts/10000, clear = F,
                       format = "[:bar] (:percent), ETA: :eta")
pb$tick(0)
ninfections2 <- 0
for (b in 1:nbursts) {
  node <- grid2$get(pos, '.')
  # new direction
  if      (node == '#')   direction <- c(-direction[2], direction[1])
  else if (node == '.')   direction <- c(direction[2], -direction[1])
  else if (node == 'F')   direction <- -direction
  # new node state
  if      (node == '.')   grid2$set(pos, 'W')
  else if (node == 'W') { grid2$set(pos, '#'); ninfections2 <- ninfections2 + 1 }
  else if (node == '#')   grid2$set(pos, 'F')
  else                    grid2$set(pos, '.')
  pos <- pos + direction
  if (b %% 10000 == 0) pb$tick()
}
  #draw(grid)
cat("Part 2:", ninfections2, "\n")
