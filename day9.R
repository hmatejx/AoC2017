# ===========
# = Day 9   =
# ===========

# Day 9
input9 <- read.table("inputs/input9.txt", as.is = T)[1, 1]

stream <- strsplit(input9, "")[[1]]

process_garbage <- function(x, mode) {
  # scan the input for garbage
  y <- vector("character")
  garbage <- skip <- F
  for (i in 1:length(x)) {
    if (!garbage && !skip) {                # we are not in garbage or skip mode
      if (x[i] == "<") garbage <- T         # set garbage mode
      else if (mode == 1)  y <- c(y, x[i])  # valid non-garbage output
      next
    }
    if (garbage) {                          # we are in garbage mode
      if (skip) skip <- F                   # skip this character and unset skip mode
      else if (x[i] == "!") skip <- T       # set skip mode
      else if (x[i] == ">") garbage <- F    # close garbage mode
      else if (mode == 2) y <- c(y, x[i])   # valid garbage output
    }
  }  
  return(y)
}

score <- function(x) {
  level <- total <- 0
  for (i in 1:length(x)) {
    if (x[i] == "{")      total <- total + (level <- level + 1)
    else if (x[i] == "}") level <- level - 1
    else if (x[i] == ",") next
  }
  return(total)
}

# part 1
score(process_garbage(stream, 1))

# part 2
length(process_garbage(stream, 2))
