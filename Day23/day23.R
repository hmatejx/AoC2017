# ===========
# = Day 23  =
# ===========

input23 <- strsplit(readChar("input23.txt", file.size("input23.txt")), "\r\n", fixed = T)[[1]]
input23 <- lapply(input23, function(x) strsplit(x, split = " ")[[1]])
regs <- letters[1:8]

#
mul_count = 0
step <- function(prog, debug = F) {
  # debug
  if (debug) {
    cat(paste0("ptr = ", format(ptr, width = 3), ", cmd = ", format(prog[ptr], width = 12)))
    flush.console()
  }
  
  # parse command line string
  cmd <- prog[[ptr]]
  op <- cmd[1]
  options(warn = -1)
  p1 <- ifelse(!is.na(as.numeric(cmd[2])), as.numeric(cmd[2]), state[cmd[2]])
  p2 <- ifelse(length(cmd) > 2, ifelse(!is.na(as.numeric(cmd[3])), as.numeric(cmd[3]), state[cmd[3]]), NA)
  options(warn = 0)
  jmp <- 1
  
  # execute the operation
  if      (op == "set") state[cmd[2]] <<- p2
  else if (op == "sub") state[cmd[2]] <<- state[cmd[2]] - p2
  else if (op == "mul") { state[cmd[2]] <<- state[cmd[2]] * p2; mul_count <<- mul_count + 1 }
  else if (op == "jnz" && p1 != 0) jmp <- p2
  
  # debug
  if (debug) {
    cat(paste0("\tstate = [", paste(state, collapse = ", "), "]\n"))
    flush.console()
  }
  
  # increment pointer  
  ptr <<- ptr + jmp
}

# Part 1
state <- rep(0, length(regs))
names(state) = regs
ptr <- 1
while (ptr <= length(input23)) {
  step(input23)
}

# Part 2
# Analysis of code from https://www.reddit.com/r/adventofcode/comments/7lms6p/comment/droj4mo/
# shows that the program calculates which numbers between 100*a + 100000 and 100*a + 117000 
# are not prime, if numbers are incremented by 17 in each iteration
is_prime <- function(x) {
  for (i in 2:sqrt(x)) if ((x %% i) == 0) return(0)
  return(1)
}
a <- 65
res <- sapply(seq(100*a + 100000, 100*a + 117000, by = 17), is_prime)
print(paste0("Part 2: ", sum(res == 0)))
