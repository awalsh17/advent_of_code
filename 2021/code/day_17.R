# Day 17 in R

# Input ----
# lazy - just type here
input <- c(185, 221, -122, -74) #target area: x=185..221, y=-122..-74
test_input <- c(20, 30, -10, -5) #x=20..30, y=-10..-5
# Part 1 instructions ----
# return the max y
# Solve ----
shoot_probe <- function(x, y, input){
  pos <- c(0,0)
  # get the steps before you fall out y
  n_steps <- 2*y + 0.5*(-input[3])
  # Get all the x pos
  x_v <- cumsum(x:0)
  if (length(x_v) > n_steps) {
    n_steps <- length(x_v)
  } else {
    x_v <- c(x_v, rep(x_v[length(x_v)], n_steps - length(x_v)))
  }
  # get all the y pos
  y_v <- cumsum(seq(from = y, by = -1, length.out = n_steps))

  is_inside <- dplyr::between(x_v, input[1], input[2]) &
    dplyr::between(y_v, input[3], input[4])
  if (any(is_inside)) return(max(y_v)) else return(-Inf)
}
# tests
shoot_probe(6, 3, test_input)
shoot_probe(6, 9, test_input)
shoot_probe(110, 20, input)

maxy <- 0
for (x in 1:222){ # limits based on input values
  for (y in -123:500){
    newy = shoot_probe(x, y, input)
    if(newy > maxy) maxy = newy
  }
}

# 7381

?optim

# Part 2 ----
# How many distinct initial velocity values cause the probe to be within the target area after any step?
# Solve ----

solve2 <- function(input){
  maxy <- 0
  n_good <- 0
  for (x in 1:(input[2] + 10)){
    for (y in (input[3]-1):(input[3]*-2)){
      newy = shoot_probe(x, y, input)
      if(newy > maxy) maxy = newy
      if(!is.infinite(newy)) n_good = n_good + 1
    }
  }
  return(n_good)
}
solve2(test_input)
solve2(input)
# 3019

# there is probably a two line solution to this one. lol.
