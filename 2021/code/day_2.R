# Day 2 in R


# Input ----
input <- readLines(here::here('input/day2/input.txt'))


# Part 1 instructions ----
# You are driving a sub
# input is your course
# What do you get if you multiply your final horizontal position by your final depth?

# Solve ----
test_input <- c(
  "forward 5",
  "down 5",
  "forward 8",
  "up 3",
  "down 8",
  "forward 2"
)
# test answer is 150
library(dplyr)
solve1 <- function(input){
  totals <- data.frame(raw=input) %>%
    tidyr::separate(raw, c("dir","amt"),sep="\\s") %>%
    group_by(dir) %>%
    summarise(total = sum(as.integer(amt)), .groups = "drop")
  totals$total[totals$dir=="forward"]*(totals$total[totals$dir=="down"]-totals$total[totals$dir=="up"])
}
solve1(test_input)
solve1(input)

# Part 2 ----
# down X increases your aim by X units.
# up X decreases your aim by X units.
# forward X does two things:
#   It increases your horizontal position by X units.
# It increases your depth by your aim multiplied by X.

# Solve ----
# there is probably an elegant two line solution
solve2 <- function(input){
  totals <- data.frame(raw=input) %>%
    tidyr::separate(raw, c("dir","amt"),sep="\\s") %>%
    mutate(amt = as.integer(amt))
  # vector of horiz, depth, aim
  pos <- c(0,0,0)
  for (i in 1:nrow(totals)){
    if(totals[i,]$dir == "down") pos[3] <- pos[3] + totals[i,]$amt
    if(totals[i,]$dir == "up") pos[3] <- pos[3] - totals[i,]$amt
    if(totals[i,]$dir == "forward"){
      pos[1] <- pos[1] + totals[i,]$amt
      pos[2] <- pos[2] + pos[3]*totals[i,]$amt
    }
  }
  pos[1] * pos[2]
}

solve2(test_input)
solve2(input)
