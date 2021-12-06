# Day 6 in R
# I actually did part 2 in julia!! (not R)

# Input ----
input <- as.integer(unlist(strsplit(readLines(here::here('input/day6/input.txt')),",")))
test_input <- c(3,4,3,1,2)

# Part 1 instructions ----
# simulate lanternfish

# Solve ----
solve1 <- function(input, n){
for (round in 1:n){
  # update
  input <- input - 1
  new_fish <- sum(input==(-1))
  input[input==(-1)] <- 6
  input <- c(input, rep(8, new_fish))
}
length(input)
}

solve1(test_input, 80)
solve1(input, 80)

# Part 2 ----
# run for 256 days - knew this was going to happen
# Solve ----

# I added this after learning from day_6.jl I like the julia solution better?
solve2 <- function(input, n){
  #count the n of each number
  index <- table(input)
  index["6"] <- 0
  index["7"] <- 0
  index["8"] <- 0
  index["0"] <- 0
  for (round in 1:n){
    # update
    new_fish <- index["0"]
    for (nn in as.character(0:7)){
      index[nn] <- index[as.numeric(nn)+1]
    }
    index["6"] = index["6"] + new_fish
    index["8"] = new_fish
  }
  return(sum(index, na.rm = T))
}

solve2(input, 256)
