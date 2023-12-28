# ===========
# = Day 20  =
# ===========

input20 <- as.data.frame(
  t(sapply(strsplit(readChar("input20.txt", file.size("input20.txt")), "\r\n")[[1]],
           function(line) { as.numeric(strsplit(gsub("p|v|a|=|<|>", "", line), ",")[[1]]) },
           USE.NAMES = F)))
names(input20) <- paste0(rep(c("p", "v", "a"), each = 3), rep(c("x", "y", "z"), 3))

# Part 1
accell <- apply(input20[, 7:9], 1, function(a) { sum(abs(a)) })
cat(which.min(accell) - 1)

# Part 2
prtcls <- input20
tick <- function() {
  prtcls[, 4:6] <<- prtcls[, 4:6] + prtcls[, 7:9]
  prtcls[, 1:3] <<- prtcls[, 1:3] + prtcls[, 4:6]
  # calculate distances between particles
  d <- dist(prtcls[, 1:3], "manhattan")
  dmat <- matrix(NA, nrow(prtcls), nrow(prtcls))
  dmat[lower.tri(dmat)] <- d
  # remove colliding particles
  ind <- unique(c(which(dmat == 0, arr.ind = T)))
  if (length(ind) > 0) {
    cat("Bang! Removing particles", rownames(prtcls)[ind], "\n")
    prtcls <<- prtcls[-ind, ]
  }
  return(min(d))
}

repeat {
  if (tick() > 1e5) break
}
cat(nrow(prtcls), "\n")
