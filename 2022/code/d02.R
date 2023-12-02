# --- Day 2: Rock Paper Scissors ---

part1 <- read.table("input/input02.txt", header = FALSE)
dummy1 <- read.table("input/dummy02.txt")

# annoying coding up rules
library(dplyr)

solve1 <- function(input) {
  input %>% 
    mutate(score = case_when(
      V1 == "A" & V2 == "X" ~ 3 + 1,
      V1 == "A" & V2 == "Y" ~ 6 + 2,
      V1 == "A" & V2 == "Z" ~ 0 + 3,
      #paper + rock
      V1 == "B" & V2 == "X" ~ 0 + 1,
      V1 == "B" & V2 == "Y" ~ 3 + 2,
      V1 == "B" & V2 == "Z" ~ 6 + 3,
      #scissors + 1
      V1 == "C" & V2 == "X" ~ 6 + 1,
      V1 == "C" & V2 == "Y" ~ 0 + 2,
      V1 == "C" & V2 == "Z" ~ 3 + 3
    )) %>% 
    summarise(answer = sum(score))
}
solve1(part1)
# first guess 12387 was too low - HAD CODE THE OPPONENt/ SELF backwards!!!

solve2 <- function(input) {
  input %>% 
    mutate(score = case_when(
      # lose, draw win
      V1 == "A" & V2 == "X" ~ 0 + 3, # scissors
      V1 == "A" & V2 == "Y" ~ 3 + 1,
      V1 == "A" & V2 == "Z" ~ 6 + 2,
      #paper beats rock
      V1 == "B" & V2 == "X" ~ 0 + 1,
      V1 == "B" & V2 == "Y" ~ 3 + 2,
      V1 == "B" & V2 == "Z" ~ 6 + 3,
      #scissors beats paper
      V1 == "C" & V2 == "X" ~ 0 + 2,
      V1 == "C" & V2 == "Y" ~ 3 + 3,
      V1 == "C" & V2 == "Z" ~ 6 + 1
    )) %>% 
    summarise(answer = sum(score))
}
solve2(part1)
