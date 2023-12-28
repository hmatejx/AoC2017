# ===========
# = Day 24  =
# ===========

library(collections)

input24 <- strsplit(readChar("input24.txt", file.size("input24.txt")), "\r\n", fixed = T)[[1]]
input24 <- lapply(input24, function(x) as.numeric(unlist(strsplit(x, split = "/")[[1]])))


chainbridge <- function() {
  chains <- data.frame(row.names = c("strength", "length"))
  front <- deque()
  front$push(list(item = c(0, 0), strength = 0, length = 0, available = input24))
  # find the next items that can be added to the bridge as the next step
  while (front$size() > 0) {
    current <- front$pop() # DFS
    # get candidate items to add
    candidate_idx <- sapply(current$available, function(x) { current$item[2] %in% x })
    # check if we are done (no fitting items left)
    if (!any(candidate_idx)) {
      chains <- rbind(chains, list(strength = current$strength, length = current$length))
    } else {
      # explore all candidate items
      candidate_items <- current$available[candidate_idx]
      for (item in candidate_items) {
        # orient the fitting item
        if (item[1] == current$item[2])
          item_ <- item
        else
          item_ <- rev(item)
        # construct the new state
        available_idx <- sapply(current$available, function(x) { !all(x == item) })
        new_state = list(item = item_,
                         strength = current$strength + sum(item),
                         length = current$length + 1,
                         available = current$available[available_idx])
        front$push(new_state)
      }
    }
  }
  return(chains)
}

chains <- chainbridge()
cat("Part 1:", max(chains$strength), "\n")
cat("Part 2:", max(chains$strength[chains$length == max(chains$length)]), "\n")
