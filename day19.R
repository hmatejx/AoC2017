# ===========
# = Day 19  =
# ===========

input19 <- t(sapply(strsplit(readChar("inputs/input19.txt", file.size("inputs/input19.txt")), "\r\n", fixed = T)[[1]],
             function(line) { strsplit(line, "", fixed = T)[[1]] }, USE.NAMES = F))

# walk around
end <- F
repeat {
  if (end) break
}
