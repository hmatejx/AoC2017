# ===========
# = Day 12  =
# ===========

input12 <- lapply(strsplit(readChar("inputs/input12.txt", file.size("inputs/input12.txt")), "\r\n")[[1]], 
                  function(x) { 
                    y <- strsplit(x, " <-> ", fixed = T)[[1]]
                    pipes = as.numeric(strsplit(y[2], ", ", fixed = T)[[1]])
                  })

# Depth-first search
dfs <- function(node) {
  for (con in input12[[node + 1]]) {
    if (visited[con + 1] == 0) {
      visited[con + 1] <<- 1
      dfs(con)
    }
  }
}

# Part 1
visited <- c(1, rep(0, length(input12) - 1))
dfs(0)
cat(sum(visited))

# Part 2
visited <- rep(0, length(input12))
groups <- 0
for (node in 0:(length(input12) - 1)) {
  if (visited[node + 1] == 0) {
    visited[node + 1] <- 1
    groups <- groups + 1
    dfs(node)
  }
}
cat(groups)
