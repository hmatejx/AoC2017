# ===========
# = Day 11  =
# ===========

input11 <- strsplit(readChar("inputs/input11.txt", file.size("inputs/input11.txt")), ",", fixed = T)[[1]]

# hex distance formula
hex_distance <- function(a, b) {
  (abs(a$q - b$q) + abs(a$q + a$r - b$q - b$r) + abs(a$r - b$r)) / 2
}

# initial state
max_distance <- 0
start <- list(q = 0, r = 0)

walk <- function(origin, moves) {
  pos <- unlist(origin)
  lapply(moves, function(move) {
    pos <<- pos + switch(move,
      n  = c( 0, -1),
      ne = c(-1,  0),
      se = c(-1,  1),
      s  = c( 0,  1),
      sw = c( 1,  0),
      nw = c( 1, -1)
    )
    dist <- hex_distance(origin, as.list(pos))
    if (dist > max_distance) max_distance <<- dist
  })
  return(as.list(pos))
}

end <- walk(start, input11)
end_distance <- hex_distance(start, end)
