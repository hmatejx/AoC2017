# ===========
# = Day 14  =
# ===========

# load Day 10 to get knot hash
source("day10.R"); rm(input10)

input14 <- "uugsqrei"

flood_fill <- function(i, j, r) {
  # skip empty field or fields already in the region
  if (disk[i, j] == 0) return()
  
  # set the field to belong to region r
  region[i, j] <<- r
  
  # find all neighbours not yet in the group and start filling there
  if (i > 1 && disk[i - 1, j] && region[i - 1, j] != r)
    flood_fill(i - 1, j, r)
  if (i < 128 && disk[i + 1, j] && region[i + 1, j] != r)
    flood_fill(i + 1, j, r)
  if (j > 1 && disk[i, j - 1] && region[i, j - 1] != r)
    flood_fill(i, j - 1, r)
  if (j < 128 && disk[i, j + 1] && region[i, j + 1] != r)
    flood_fill(i, j + 1, r)
}

# Day 1
disk <- sapply(0:127, function(row) { hash(paste0(input14, "-", row), binary = T) } )
cat(sum(disk))

# Day 2
region <- matrix(0, nrow = 128, ncol = 128)
nr <- 1
for (i in 1:128) {
  for (j in 1:128) {
    if (disk[i, j] && region[i, j] == 0) {
      flood_fill(i, j, nr)
      nr <- nr + 1
    }
  }
}
cat(max(region))

# create pretty image
image(region, col = c("white", rainbow(15))[0:max(region) %% 16 + 1])
