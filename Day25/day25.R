# ===========
# = Day 25  =
# ===========

library(progress)

state <- 'A'
tape <- c(0)
pos <- 1

move_right <- function() {
  pos <<- pos + 1
  # extend tape on right if necessary
  if (pos > length(tape)) {
    tape <<- append(tape, 0)
  }  
}

move_left <- function() {
  pos <<- pos - 1
  # extend tape on left if necessary
  if (pos < 1) {
    tape <<- c(0, tape)
    pos <<- pos + 1
  }
}

step <- function() {
  if (state == 'A') {
    if (tape[pos] == 0) {
      tape[pos] <<- 1
      move_right()
      state <<- 'B'
    }
    else if (tape[pos] == 1) {
      tape[pos] <<- 0
      move_left()
      state <<- 'E'
    }
  }
  else if (state == 'B') {
    if (tape[pos] == 0) {
      tape[pos] <<- 1
      move_left()
      state <<- 'C'
    }
    else if (tape[pos] == 1) {
      tape[pos] <<- 0
      move_right()
      state <<- 'A'
    }
  }
  else if (state == 'C') {
    if (tape[pos] == 0) {
      tape[pos] <<- 1
      move_left()
      state <<- 'D'
    }
    else if (tape[pos] == 1) {
      tape[pos] <<- 0
      move_right()
      state <<- 'C'
    }
  }
  else if (state == 'D') {
    if (tape[pos] == 0) {
      tape[pos] <<- 1
      move_left()
      state <<- 'E'
    }
    else if (tape[pos] == 1) {
      tape[pos] <<- 0
      move_left()
      state <<- 'F'
    }
  }
  else if (state == 'E') {
    if (tape[pos] == 0) {
      tape[pos] <<- 1
      move_left()
      state <<- 'A'
    }
    else if (tape[pos] == 1) {
      tape[pos] <<- 1
      move_left()
      state <<- 'C'
    }
  }
  else if (state == 'F') {
    if (tape[pos] == 0) {
      tape[pos] <<- 1
      move_left()
      state <<- 'E'
    }
    else if (tape[pos] == 1) {
      tape[pos] <<- 1
      move_right()
      state <<- 'A'
    }
  }
}

steps <- 12386363
pb <- progress_bar$new(total = steps/100000, clear = F,
                       format = "[:bar] (:percent), ETA: :eta")
pb$tick(0)
for (i in 1:steps) {
  step()
  if (i %% 100000 == 1) pb$tick()
}
cat("Part 1:", sum(tape), "\n")
