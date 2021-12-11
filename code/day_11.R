# Day 11 in R

# Input ----
input <- read.fwf(here::here("input/day11/input.txt"), widths = rep(1, 10))
test_input <- read.fwf(here::here("input/day11/test_input.txt"), widths = rep(1, 10))

# Part 1 instructions ----
# I think I can use matrix convolution
# but all we care about is total flashes, so I might be overthinking it.
# Solve ----
do_step <- function(input) {
  flashes <- 0
  flash_mat <- matrix(0, 10, 10)

  # add one
  input <- input + 1
  # binarize
  new_bin <- input
  new_bin[new_bin <= 9] <- 0
  new_bin[new_bin > 9] <- 1

  # If zeros - then step over
  while (sum(new_bin) > 0) {

    flashes <- flashes + sum(new_bin)
    # store flashed spots
    flash_mat[input > 9] <- 1

    to_add <- OpenImageR::convolution(new_bin,
                                      matrix(c(1, 1, 1, 1, 0, 1, 1, 1, 1), 3, 3))

    input <- input + to_add

    # Find only the NEW 10+ values
    new_bin2 <- input
    new_bin2[new_bin2 <= 9] <- 0
    new_bin2[new_bin2 > 9] <- 1
    new_bin <- new_bin2 - flash_mat
  }
  # update the matrix
  input[input > 9] <- 0
  return(list(input, flashes))
}


solve1 <- function(input) {
  res <- list(input, 0)
  total_flash <- 0
  for (i in seq_len(100)) {
    res <- do_step(res[[1]])
    total_flash <- total_flash + res[[2]]
  }
  return(total_flash)
}

solve1(input)

# Part 2 ----
# What is the first step during which all octopuses flash?
# Solve ----

solve2 <- function(input) {
  res <- list(input, 0)
  step <- 0
  while (res[[2]] != 100) {
    res <- do_step(res[[1]])
    step <- step + 1
  }
  return(step)
}

solve2(input)
