# Day 25 in R

# Input ----
in_width <- nchar(readLines(here::here('input/day25/input.txt'), n = 1L))
input <- as.matrix(read.fwf(here::here('input/day25/input.txt'),
                  widths = rep(1,in_width)))

test <- as.matrix(read.fwf(here::here('input/day25/test.txt'),
                 widths = rep(1,10)))

# Part 1 instructions ----
# What is the first step on which no sea cucumbers move?
# Solve ----

make_step <- function(state, width, height) {
  shortw <- width-1
  shorth <- height-1
  cur_step <- state

  next_east <- cbind(cur_step[,2:width], cur_step[,1]) == "."
  moves_east <- cur_step == ">" & next_east

  after_east <- cur_step
  after_east[moves_east] <- "."
  after_east[cbind(moves_east[,width], moves_east[,1:shortw])] <- ">"

  next_south <- rbind(after_east[2:height,], after_east[1,]) == "."
  moves_south <- after_east == "v" & next_south

  after_south <- after_east
  after_south[moves_south] <- "."
  after_south[rbind(moves_south[height,], moves_south[1:shorth,])] <- "v"

  return(after_south)
}

make_step(test, 10, 9) #test

# check for next moves then move the pieces
# move until n == n+1
solve1 <- function(input) {
  w <- ncol(input)
  h <- nrow(input)
  n <- 0
  new_step <- input
  repeat{
    past_step <- new_step
    new_step <- make_step(past_step, w, h)
    n <- n + 1
    if (all(past_step == new_step)) break
  }
  return(n)
}

solve1(input)

# Part 2 ----

# Solve ----

