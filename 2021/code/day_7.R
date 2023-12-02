# Day 7 in R


# Input ----
input <- as.integer(strsplit(readLines(here::here('input/day7/input.txt')),",")[[1]])
test_input <- c(16,1,2,0,4,2,7,1,2,14)

# Part 1 instructions ----
# yeah, optimization
# answer is the cost, not the position. ugh.

# Solve ----
cost_function <- function(pos, input){
  sum(abs(input - pos))
}
solve1 <- function(input){
  # you could write the algorithm, or just be lazy
  optimize(cost_function,
         interval = c(min(input),max(input)),
         input)$objective
}

solve1(test_input)
solve1(input)

# Part 2 ----
# new function
# Solve ----

cost_function2 <- function(pos, input){
  triangle_nums <- cumsum(seq(max(input)+1)-1)[-1]
  sum(triangle_nums[abs(input - pos)])
}
solve2 <- function(input){
  # you could write the algorithm, or just be lazy
  min_val <- round(optimize(cost_function2,
           interval = c(min(input),max(input)),
           input)$minimum)
  cost_function2(min_val, input)

}
solve2(test_input)
solve2(input)

# This one could have been easier if I had read the directions correctly.
# This question is easier than I was making it - annoying!
sum(abs(input - median(input))) # part 1 answer
# part 2... no need to actually optimize.
min(sapply(0:max(input), cost_function2, input)) #part 2 answer
