# Day 3 in R
library(dplyr) # used dplyr but no need

# Input ----
input <- readLines(here::here('input/day3/input.txt'))

# Part 1 instructions ----
# calculate the gamma rate and epsilon rate, then multiply them together. What is the power consumption of the submarine? (Be sure to represent your answer in decimal, not binary.)
test_input <- c(
  "00100",
  "11110",
  "10110",
  "10111",
  "10101",
  "01111",
  "00111",
  "11100",
  "10000",
  "11001",
  "00010",
  "01010"
)
# Solve ----
# need to get the most and least common at each position
# then convert to decimal
solve1 <- function(input){
  new_in <- data.frame(raw=input) %>%
    tidyr::separate(raw, into=paste0("x",0:nchar(input[1])),sep="") %>%
    select(-x0) %>%
    summarise(across(everything(),
                     list(~ifelse(sum(.x==0)>sum(.x==1),0,1))
                     ))
    gamma = paste0(new_in[1,],collapse = "")
    new_in[2,] <- sapply(new_in[1,], function(x) ifelse(x==1,0,1))
    epsilon = paste0(new_in[2,],collapse = "")
    strtoi(gamma, base = 2) * strtoi(epsilon, base = 2)
}

solve1(test_input)
solve1(input)
# Part 2 ----
# recursive
# Solve ----

# right function to filter based on most common bit at given position

filter_input <- function(input, pos, dir = "most"){
  if (dir =="most"){
    most_common = input %>%
      summarise(across(everything(),
                       list(~ifelse(sum(.x==0)>sum(.x==1),0,1))
      ))
  }
  if (dir =="least"){
    most_common = input %>%
      summarise(across(everything(),
                       list(~ifelse(sum(.x==0)>sum(.x==1),1,0))
      ))
  }

  row_keep <- input[,pos]==most_common[,pos]
  input[row_keep,]
}

solve2 <- function(input){
input_df <- data.frame(raw=input) %>%
  tidyr::separate(raw, into=paste0("x",0:nchar(input[1])),sep="") %>%
  select(-x0)

rating_1 = input_df
i <- 1
while (nrow(rating_1)>1){
  rating_1 <- filter_input(rating_1, i)
  i <- i+1
}
rating_2 <- input_df
i <- 1
while (nrow(rating_2)>1){
  rating_2 <- filter_input(rating_2, i, dir="least")
  i <- i+1
}

strtoi(paste0(rating_1,collapse = ""), base = 2) * strtoi(paste0(rating_2,collapse = ""), base = 2)
}

solve2(test_input)
solve2(input)

# After I finished, I thought of some ways this could be done better or easier
# 1: reading in the input as table to begin with
input <- read.fwf(here::here('input/day3/input.txt'), widths=rep(1,12))
# 2: find most/least common digit with colsums - TRUE means 1 or tie
colSums(input) > (nrow(input)/2-1)
# to make binary again
paste(ifelse(colSums(input) > (nrow(input)/2-1), 1, 0), collapse = "")
