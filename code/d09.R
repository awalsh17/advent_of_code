# --- Day 9 ---

real <- read.table("input/input09.txt")
dummy <- read.table("input/dummy09b.txt")

# How many positions does the tail of the rope visit at least once?
# ideas: track coordinates visited?
solve1 <- function(input){
  visited_tail <- list(c(1,1))
  # expand the input to cover every single move
  moves <- unlist(tidyr::uncount(input, V2))
  current <- c(1, 1)
  move_codes <- list("R" = c(1,0), "L" = c(-1,0), "U" = c(0,1), "D" = c(0,-1))
  for (i in moves) {
    last <- current
    current <- current + move_codes[[i]]
    # when dist < 2 - OK
    # otherwise, move to last place current was
    dist_head <- sqrt(sum((current - visited_tail[[1]])^2))
    if (dist_head >= 2){
      visited_tail <- c(list(last), visited_tail)
    }
  }
  length(unique(visited_tail))
}
solve1(real) # had a typo that was bad, but fixed it!

# this is not going to be an easy mod - now the rope is 9 long 
# I just just track all 9?
move_codes <- list("R" = c(1,0), "L" = c(-1,0), "U" = c(0,1), "D" = c(0,-1))
solve2 <- function(input) {
  # expand the input to cover every single move
  moves <- unlist(tidyr::uncount(input, V2))
  all_positions <- list(c(1,1), c(1,1), c(1,1), c(1,1), c(1,1), c(1,1), c(1,1), c(1,1), c(1,1), c(1,1))
  visited_tail <- list(c(1,1))

  for (i in moves) {
    last <- all_positions
    all_positions[[1]] <- all_positions[[1]] + move_codes[[i]]
    # when dist < 2 - OK
    # otherwise, move to last place head_position was
    for (knot in 2:10) {
      diff_prior <- all_positions[[knot - 1]] - all_positions[[knot]]
      if (any(abs(diff_prior) > 1)){
        all_positions[[knot]] <- last[[knot]] + sign(diff_prior)
      }
    }
    # update tail pos list
    visited_tail <- c(visited_tail, list(all_positions[[10]]))
  }
  length(unique(visited_tail))
  # visited_tail
}
solve2(real)
# note - the logic for how the rope moved took me a long time to understand

