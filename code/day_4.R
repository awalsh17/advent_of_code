# Day 4 in R

library(dplyr)
# Input ----
# need to parse into the numbers (line 1)
# and then the boards (fixed width sep by blank lines)
input_calls <- unlist(strsplit(readLines(here::here('input/day4/input.txt'))[1],","))
# this is harder in R than it needs to be (I am sure someone had a better idea)
input <- read.fwf(here::here('input/day4/input.txt'),
                  skip = 1,
                  widths = rep(3,5)) %>%
  mutate(types = ifelse(is.na(V1), "new","board")) %>%
  group_by(types) %>%
  mutate(index = ifelse(types=="new",1:n(),NA_integer_)) %>% ungroup() %>%
  tidyr::fill(index, .direction = "down") %>%
  filter(types=="board") %>% select(-types)

# Part 1 instructions ----
# It's bingo
# The score of the winning board can now be calculated. Start by finding the sum of all unmarked numbers on that board; in this case, the sum is 188. Then, multiply that sum by the number that was just called when the board won, 24, to get the final score, 188 * 24 = 4512.
# Solve ----
# a function to calc the score
calc_score <- function(board, last_n){
  sum(board)*as.numeric(last_n)
}
# need a function to find the winning board
play_game <- function(board, calls){
  turn <- 1
  won <- FALSE
  while (!won){
    matching_spot <- board==calls[turn]
    if (any(matching_spot)){
      board[matching_spot] <- 0
      if(any(colSums(board)==0) | any(rowSums(board)==0)){
        won <- TRUE
        return(data.frame(turn=turn,score=calc_score(board,calls[turn])))
      }
    }
    turn <- turn + 1
  }
}

# put it all together
input %>%
  tidyr::nest(-index) %>%
  mutate(turn_won = purrr::map(data, play_game, input_calls)) %>%
  tidyr::unnest(turn_won) %>%
  arrange(turn) %>% head(1)

# Part 2 ----
# Same but the LAST board - ok, this was the easiest part 2 so far!
# Solve ----

input %>%
  tidyr::nest(-index) %>%
  mutate(turn_won = purrr::map(data, play_game, input_calls)) %>%
  tidyr::unnest(turn_won) %>%
  arrange(desc(turn)) %>% head(1) # just changed to desc()
