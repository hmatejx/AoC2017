# ===========
# = Day 3   =
# ===========

# input
input3 <- 347991

# part 1
spiral <- function(n) {
  k <- ceiling((sqrt(n) - 1)/2); t <- 2*k + 1; m <- t^2; t <- t - 1
  if (n >= m - t) return(c(k - (m - n), -k)) else m = m - t
  if (n >= m - t) return(c(-k, -k + (m - n))) else m = m - t
  if (n >= m - t) return(c(-k + (m - n), k)) else return(c(k, k - (m - n -t)))
}
cat(sum(abs(spiral(input3))), "\n")

# part 2
s <- read.table("https://oeis.org/A141481/b141481.txt", skip = 2)[, 2]
cat(s[s > input3][1], "\n")
