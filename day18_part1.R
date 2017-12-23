# ===========
# = Day 18  =
# ===========

input18 <- strsplit(readChar("inputs/input18.txt", file.size("inputs/input18.txt")), "\r\n", fixed = T)[[1]]

# extract registers
regs <- unique(unlist(sapply(input18, function(i) {
  instr <- strsplit(i, " ")[[1]]
  if (instr[1] %in% c("set", "add", "mul", "mod"))
    return(instr[2])
})))
state <- rep(0, length(regs) + 2)
names(state) <- c(regs, "freq", "rcv")

#
step <- function(prog) {
  # debug
  cat(paste0("ptr = ", format(ptr, width = 3), ", cmd = ", format(prog[ptr], width = 12)))
  
  # parse command line string
  cmd <- strsplit(prog[ptr], " ")[[1]]
  op <- cmd[1]
  options(warn = -1)
  p1 <- ifelse(!is.na(as.numeric(cmd[2])), as.numeric(cmd[2]), state[cmd[2]])
  p2 <- ifelse(length(cmd) > 2, ifelse(!is.na(as.numeric(cmd[3])), as.numeric(cmd[3]), state[cmd[3]]), NA)
  options(warn = 0)
  jmp <- 1
  
  # execute the operation
  if (op == "snd") state["freq"] <<- state[cmd[2]]
  else if (op == "set") state[cmd[2]] <<- p2
  else if (op == "add") state[cmd[2]] <<- state[cmd[2]] + p2
  else if (op == "mul") state[cmd[2]] <<- state[cmd[2]] * p2
  else if (op == "mod") state[cmd[2]] <<- state[cmd[2]] %% p2
  else if (op == "rcv" && p1 > 0) state["rcv"] <<- state["freq"]
  else if (op == "jgz" && p1 > 0) jmp <- p2
  
  # debug
  cat(paste0("\tstate = [", paste(state, collapse = ", "), "]\n"))

  # increment pointer  
  ptr <<- ptr + jmp
}

# Part 1
ptr <- 1
while (ptr > 0 && ptr <= length(input18) && state["rcv"] == 0) {
  step(input18)
}