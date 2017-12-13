# ===========
# = Day 8   =
# ===========

input8 <- read.table("inputs/input8.txt", as.is = T)[, -4]
names(input8) <- c("reg", "cmd", "par", "cndreg", "cnd", "val")

# Part 1 & 2
cmds <- input8
regnames <- unique(c(cmds$reg, cmds$cndreg))
nreg <- length(regnames)
regs <- as.data.frame(as.list(rep(0, nreg)), col.names = regnames)

step <- function(addr) {
  # get instruction line
  line <- cmds[addr, ]
  
  with(line, {
    # check if condition is fullfilled
    res <- F
    if (cnd == "!=")      res <- regs[[cndreg]] != val
    else if (cnd == ">=") res <- regs[[cndreg]] >= val
    else if (cnd == ">")  res <- regs[[cndreg]] >  val
    else if (cnd == "==") res <- regs[[cndreg]] == val
    else if (cnd == "<=") res <- regs[[cndreg]] <= val
    else                  res <- regs[[cndreg]] <  val
    
    # perform command
    if (res) {
      if (cmd == "inc") regs[[reg]] <<- regs[[reg]] + par
      else              regs[[reg]] <<- regs[[reg]] - par
    }
  })
  
  max(regs)
}
max(sapply(1:nrow(cmds), step)) # Part 2
max(regs)                       # Part 1
