# ===========
# = Day 18  =
# ===========

input18 <- strsplit(readChar("input18.txt", file.size("input18.txt")), "\r\n", fixed = T)[[1]]

# extract registers
regs <- unique(unlist(sapply(input18, function(i) {
  instr <- strsplit(i, " ")[[1]]
  if (instr[1] %in% c("set", "add", "mul", "mod", "snd", "rcv") && instr[2] %in% letters)
    return(instr[2])
})))

# initial state
state <- matrix(rep(0, 2*length(regs)), nrow = 2)
colnames(state) <- regs
state[2, "p"] <- 1

step <- function(prog) {
  # get this and other program id
  id <- running + 1
  other_id <- 2 - running
  
  # debug
  cat(paste0("id = ", running, ", ptr = ", format(ptr[id], width = 3), 
             ", cmd = ", format(prog[ptr[id]], width = 12),
             ", queue = [", format(paste(queue[[id]][seq(1, length.out = min(length(queue[[id]]), 3))], 
                                         collapse = ", "), width = 16), "...]"))

  # parse command line string
  cmd <- strsplit(prog[ptr[id]], " ")[[1]]
  op <- cmd[1]
  options(warn = -1)
  p1 <- ifelse(!is.na(as.numeric(cmd[2])), as.numeric(cmd[2]), state[id, cmd[2]])
  p2 <- ifelse(length(cmd) > 2, ifelse(!is.na(as.numeric(cmd[3])), as.numeric(cmd[3]), state[id, cmd[3]]), NA)
  options(warn = 0)
  jmp <- 1
  
  # execute the operation
  if (op == "snd") {
    queue[[other_id]] <<- c(queue[[other_id]], p1)
    snd_counter[id] <<- snd_counter[id] + 1
  } else if (op == "set") state[id, cmd[2]] <<- p2
  else if (op == "add") state[id, cmd[2]] <<- state[id, cmd[2]] + p2
  else if (op == "mul") state[id, cmd[2]] <<- state[id, cmd[2]] * p2
  else if (op == "mod") state[id, cmd[2]] <<- state[id, cmd[2]] %% p2
  else if (op == "rcv") {
    if (length(queue[[id]]) > 0) {
      # pop queue
      state[id, cmd[2]] <<- queue[[id]][1]
      queue[[id]] <<- queue[[id]][-1]
    } else {
      # empty queue, switch to other program
      jmp <- 0
    }
  }
  else if (op == "jgz" && p1 > 0) jmp <- p2
  
  # debug
  cat(paste0(", jmp = ", jmp, ", state = [", paste(state[id, ], collapse = ", "), "]\n"))

  # increment pointer  
  ptr[id] <<- ptr[id] + jmp
  
  # this program is waiting for input, swith to other program
  if (jmp == 0) {
    waiting[id] <<- T
    running <<- 1 - running
  }
}

# Part 2
queue <- list(vector("numeric"), vector("numeric"))
ptr <- c(1, 1)
waiting <- c(F, F)
snd_counter <- c(0, 0)
running <- 0
while (ptr[1] > 0 && ptr[1] <= length(input18) && 
       ptr[2] > 0 && ptr[2] <= length(input18) &&
       !(waiting[1] && length(queue[[1]]) == 0 && waiting[2] && length(queue[[2]]) == 0)) {
  step(input18)
}
cat(snd_counter[2])
