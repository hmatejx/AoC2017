# ===========
# = Day 7   =
# ===========

# Day 7
ncols <- max(count.fields("input7.txt"))
input7 <- read.table("input7.txt", header = F, fill = T, as.is = T, colClasses = rep("character", ncols))[, -3]
names(input7) <- c("pprog", "weight", paste0("cprog", 1:(ncols - 3)))
input7$weight <- as.integer(gsub("\\(|\\)", "", input7$weight))
for (col in paste0("cprog", 1:(ncols - 4))) {
  input7[, col] <- gsub(",", "", input7[, col], fixed = T)
}

# Part 1
prgs <- input7
top <- prgs$pprog[which(!(prgs$pprog %in% unique(as.character(t(prgs[, 3:9])))[-1]))]

# Part 2
weight <- function(prg) {
  # get own weight
  own_weight <- prgs$weight[prgs$pprog == prg]
  
  # find all child programs
  children <- prgs[prgs$pprog == prg, 3:9]
  children <- children[children != ""]
  
  # found the top
  if (length(children) == 0) return(own_weight)
  
  # calculate the weights of all child programs
  child_weights <- sapply(children, function(x) { prgs$weight[prgs$pprog == x] })
  total_weights <- sapply(children, weight)
  
  # check total weights for balance
  if (length(unique(total_weights)) != 1) {
    cat(paste0("Program >> ", prg, " << is unbalanced!\n"))
    cat(" Child:   ", paste0(format(children, width = 10), "\t"), "\n")
    cat(" Weights: ", paste0(format(paste0(total_weights, "(", child_weights, ")"), width = 10), "\t"), "\n")
  }
  
  # return the own weight + that of all children
  return(sum(total_weights) + own_weight)
}
weight(top)
