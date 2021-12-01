# Day 1 in R

# Input ----
input <- readLines(here::here('input/day1/input.txt'))
# make sure these are numbers
input <- as.integer(input)

test_input <- c(
  199,
  200,
  208,
  210,
  200,
  207,
  240,
  269,
  260,
  263
)
# Part 1 instructions ----
# test_answer <- 7
# count the number of times a depth measurement increases from the previous measurement. (There is no measurement before the first measurement.)

# Solve ----
# we can do this a lot of ways
# honestly, simply loop and tally
tally <- 0
i <- input[1]
for (j in input[2:length(input)]){
  if (j > i) {
    tally <- tally + 1
  }
  i <- j
}

# alternative: you could also do it tidyversy
library(tidyverse)
annotated_data <- data.frame(vals = input) %>%
  mutate(direction = ifelse(vals>lag(vals),
                            "increased",
                            "decreased"))
# this is the answer
sum(annotated_data$direction=="increased", na.rm = TRUE)

# Part 2 ----
# count the number of times the sum of measurements in this sliding window increases
# n=3 sliding window
# test_answer <- 5

# Solve ----

# basic
n <- 3 # window size
tally <- 0
i <- input[1:n]
for (j in input[(n+1):length(input)]){
  prev_sum <- sum(i)
  i <- c(i[2:n],j)
  current_sum <- sum(i)
  if (current_sum > prev_sum) {
    tally <- tally + 1
  }
}


# alternative: use RcppRoll
annotated_data <- data.frame(vals = input) %>%
  mutate(
    group_total = RcppRoll::roll_sum(vals , 3,
                                     align = "right",
                                     fill = NA),
    direction = ifelse(group_total>lag(group_total),
                            "increased",
                            "decreased"))
# this is the answer
sum(annotated_data$direction=="increased", na.rm = TRUE)
