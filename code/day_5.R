# Day 5 in R

# Input ----
# parse input - x,y coords for begin and end of line
input <- readLines(here::here('input/day5/input.txt'))
test_input <- readLines(here::here('input/day5/test_input.txt'))

# Part 1 instructions ----
# Consider only horizontal and vertical lines. At how many points do at least two lines overlap?

# Solve ----
# matrix stuff?
# for each line - record all the spaces it covers

solve1 <- function(input){
  init_matrix <- matrix(0, nrow=1001, ncol=1001) #lazy

  for (line in input){
    i <- unlist(strsplit(line," -> ", fixed = TRUE))
    i <- as.integer(unlist(sapply(i, function(x) strsplit(x,","))))
    slope_x <- sign(i[3]-i[1])
    slope_y <- sign(i[4]-i[2])
    if(slope_x ==0 | slope_y==0){
      while(any(i[1:2] != i[3:4])){
        init_matrix[i[1]+1,i[2]+1] <- 1 + init_matrix[i[1]+1,i[2]+1] #1-index
        i[1] <- i[1]+slope_x
        i[2] <- i[2]+slope_y
      }
      init_matrix[i[1]+1,i[2]+1] <- 1 + init_matrix[i[1]+1,i[2]+1]
    }
  }
  sum(init_matrix>1)
}

solve1(test_input)
solve1(input)

# Part 2 ----
# just add diagonals
# Solve ----
solve2 <- function(input){
  init_matrix <- matrix(0, nrow=1001, ncol=1001)

  for (line in input){
    i <- unlist(strsplit(line," -> ", fixed = TRUE))
    i <- as.integer(unlist(sapply(i, function(x) strsplit(x,","))))
    slope_x <- sign(i[3]-i[1])
    slope_y <- sign(i[4]-i[2])
    # if(slope_x ==0 | slope_y==0){
    while(any(i[1:2] != i[3:4])){
      init_matrix[i[1]+1,i[2]+1] <- 1 + init_matrix[i[1]+1,i[2]+1]
      i[1] <- i[1]+slope_x
      i[2] <- i[2]+slope_y
    }
    init_matrix[i[1]+1,i[2]+1] <- 1 + init_matrix[i[1]+1,i[2]+1]
    # }
  }
  sum(init_matrix>1)
}
solve2(test_input)
solve2(input)

# Thoughts - There are way better ways to
# encode coordinates probably and so that annoyed me.
# I realized after that seq() would have been useful?
# I could have tracked all the x and all the y and then counted the x,y

