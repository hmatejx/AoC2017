# ===========
# = Day 19  =
# ===========

input19 <- t(sapply(strsplit(readChar("input19.txt", file.size("input19.txt")), "\r\n", fixed = T)[[1]],
             function(line) { strsplit(line, "", fixed = T)[[1]] }, USE.NAMES = F))
pipes <- input19

# walk around
end <- F
# initial position
pos <- c(which(pipes[1, ] == "|"), 1)
dir <- c(0, 1)
steps <- 0
repeat {
  if (end) break
  # get pipe element
  el <- pipes[pos[2], pos[1]]
  # continue straight up or down or left or right
  if (el == "|" || el == "-" || el %in% LETTERS) {
    if (el %in% LETTERS)
      cat(el)
    pos <- pos + dir
    # check if out of frame or dead end
    if (pos[1] < 1 || pos[1] > ncol(pipes) || pos[2] < 1 || pos[2] > nrow(pipes) ||
        pipes[pos[2], pos[1]] == " ")
      end <- T
  # turn
  } else {
    if (pipes[pos[2] + dir[1], pos[1] + dir[2]] != " ")
      dir <- c(dir[2], dir[1])
    else
      dir <- -c(dir[2], dir[1])
    pos <- pos + dir
  }

  steps <- steps + 1
}
cat("\n", steps)
