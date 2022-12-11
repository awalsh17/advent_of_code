# --- Day 8 ---
# how many trees are visible from outside the grid

dummy <- read.fwf("input/dummy08.txt", widths = rep(1, 5))
real <- read.fwf("input/input08.txt", widths = rep(1, 99))

solve1 <- function(input) {
  vis_map <- matrix(data = FALSE, nrow = nrow(input), ncol = ncol(input))
  vis_map[1, ] <- TRUE
  vis_map[, 1] <- TRUE
  vis_map[ncol(input), ] <- TRUE
  vis_map[, nrow(input)] <- TRUE
  # top-down
  for (line in 2:(ncol(input) - 1)) {
    vis <- input[line, ] > apply(input[1:(line - 1), , drop = F], 2, max)
    vis_map[line, ] <- vis_map[line, ] | vis
  }
  # down-up
  for (line in (ncol(input) - 1):2) {
    vis <- input[line, ] > apply(input[(ncol(input)):(line + 1), , drop = F], 2, max)
    vis_map[line, ] <- vis_map[line, ] | vis
  }
  # left- right
  rotate_input <- t(apply(input, 2, rev))
  for (line in 2:(ncol(rotate_input) - 1)) {
    vis <- rotate_input[line, ] > apply(rotate_input[1:(line - 1), , drop = F], 2, max)
    vis_map[, line] <- vis_map[, line] | rev(vis)
  }
  # right - left
  for (line in (ncol(rotate_input) - 1):2) {
    vis <- rotate_input[line, ] > apply(rotate_input[(ncol(rotate_input)):(line + 1), , drop = F], 2, max)
    vis_map[, line] <- vis_map[, line] | rev(vis)
  }
  sum(vis_map)
}
solve1(real)

# What is the highest scenic score possible for any tree?
# multiply viewing score in each direction
solve2 <- function(input) {
  top_score <- 0
  all_scores <- matrix(0, nrow = nrow(input), ncol = ncol(input))
  # loop through all the trees I guess
  # probably some matrix math here that could be done
  for (rowi in 1:nrow(input)) {
    for (coli in 1:ncol(input)) {
      # get the score
      right <- cumsum(
        input[rowi, coli] > input[rowi, coli:ncol(input), drop = F]
      ) == 0:(ncol(input) - coli)
      if (tail(right, 1) & (length(right) > 1)) right <- right[-1]
      right <- sum(right)
      left <- cumsum(
        rev(input[rowi, coli] > input[rowi, 1:coli, drop = F])
      ) == 0:(coli - 1)
      if (tail(left, 1) & (length(left) > 1)) left <- left[-1]
      left <- sum(left)
      top <- cumsum(
        rev(input[rowi, coli] > input[1:rowi, coli, drop = F])
      ) == 0:(rowi-1)
      if (tail(top, 1) & (length(top) > 1)) top <- top[-1]
      top <- sum(top)
      bottom <- cumsum(
        input[rowi, coli] > input[rowi:ncol(input), coli, drop = F]
      ) == 0:(ncol(input) - rowi)
      if (tail(bottom, 1) & (length(bottom) > 1)) bottom <- bottom[-1]
      bottom <- sum(bottom)

      # if edge - remove one (stupid)
      if (rowi == 1) top <- top - 1
      if (rowi == nrow(input)) bottom <- bottom - 1
      if (coli == 1) left <- left - 1
      if (coli == ncol(input)) right <- right - 1
      new_score <- right * left * top * bottom
      all_scores[rowi, coli] <- new_score
      # update
      if (new_score > top_score) top_score <- new_score
    }
  }
  top_score
}
solve2(real) # 6930 is too low
# note: I was adding and not multiplying and was very annoyed
# in general, did not try hard to be elegant here
