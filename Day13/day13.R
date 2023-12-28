# ===========
# = Day 13  =
# ===========
require(memoise)

#input13 <- list(depth = c(0, 1, 4, 6), range = c(3, 2, 4, 4))
input13 <- read.table("input13.txt", header = F, 
                      colClasses = c("character", "integer"), 
                      col.names = c("depth", "range"))
input13[, 1] <- as.integer(gsub(":", "", input13[, 1], fixed = T))

# move scanners
firewall_state <- function(time, conf = input13) {
  if (time <= 0) { 
    return(list(state = rep(1, length(conf[[1]])),
                dir   = rep(1, length(conf[[1]])),
                range = conf$range,
                depth = conf$depth))
  }

  invisible(within(firewall_state(time - 1), {
    state <- state + dir
    dir <- dir * ((state > 1 & state < range) - (state == range & dir == 1) - (state == 1 & dir == -1))
  }))
}
firewall_state <- memoise(firewall_state)

# function to print the firewall state (for debugging)
print_firewall <- function(fw, p = -1) {
  cat(paste0(" ", seq(0, max(fw$depth)), " "), "\n")
  sapply(seq(1, max(fw$range)), function(r) {
    cat(paste0(sapply(seq(0, max(fw$depth)), function(l, r) { 
      i <- which(l == fw$depth)
      ifelse(length(i) > 0 && fw$range[i] >= r,
             ifelse(fw$state[i] == r, 
                    ifelse(l == p && r == 1, "(S)", "[S]"), 
                    ifelse(l == p && r == 1, "( )", "[ ]")),
             ifelse(r == 1, ifelse(l == p && r == 1, "(.)", "..."), "   "))
    }, r = r)), "\n")
  })
  invisible()
}

# firewall traversal simulation
trip <- function(delay = 0, debug = F, fast = F) {
  
  # initial state
  packet <- -1 - delay
  caught <- 0
  trip_severity <- 0
  firewall <- firewall_state(delay)
  
  # move packet through the layers  
  for (time in delay:(max(firewall$depth) + delay)) {
    # advance packet
    packet <- packet + 1
    if (debug) {
      cat(paste0("\nPicosecond ", time, ":\n"))
      print_firewall(firewall, packet) 
    }
    # check for detection
    i <- which(firewall$depth == time - delay)
    if (length(i) > 0 && firewall$state[i] == 1) {
       if (fast) return(1)
       caught <- 1
       trip_severity <- trip_severity + firewall$depth[i]*firewall$range[i]
    }
    # advance firewall state
    firewall <- firewall_state(time + 1)
    if (debug) {
      cat("\n")
      print_firewall(firewall, packet)
    }
  }
  
  return(c(trip_severity, caught))
}
  
# Part 1
cat(trip()[1])

# Part 2
delay <- 0 # 3849742 is the solution
repeat {
  res <- trip(delay, fast = T)
  cat(paste0("delay = ", format(delay, width = 5), 
             ", caught = ", res, "\n"))
  if (res == 0) break
  delay <- delay + 2 # only even ones
}
