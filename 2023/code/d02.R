# day2
library(dplyr)
# Determine which games would have been possible if the bag had been loaded with 
# only 12 red cubes, 13 green cubes, and 14 blue cubes. What is the sum of the 
# IDs of those games?

solve1 <- function(input_path) {
  input <- readLines(input_path)
  # Get the max number of each color in each row
  reds <- stringr::str_extract_all(input, "[0-9]+(?= red)")
  greens <- stringr::str_extract_all(input, "[0-9]+(?= green)")
  blues <- stringr::str_extract_all(input, "[0-9]+(?= blue)")
  good_games <- (sapply(reds, \(x) max(as.numeric(x))) <= 12) & 
    (sapply(greens, \(x) max(as.numeric(x))) <= 13) & 
    (sapply(blues, \(x) max(as.numeric(x))) <= 14)
  sum(c(1:length(input))[good_games])
}
solve1("inputs/input2.txt")

# For each game, find the minimum set of cubes that must have been present. 
# What is the sum of the power of these sets?

solve2 <- function(input_path) {
  input <- readLines(input_path)
  # Get the max number of each color in each row
  reds <- stringr::str_extract_all(input, "[0-9]+(?= red)")
  greens <- stringr::str_extract_all(input, "[0-9]+(?= green)")
  blues <- stringr::str_extract_all(input, "[0-9]+(?= blue)")
  power <- (sapply(reds, \(x) max(as.numeric(x)))) * 
    (sapply(greens, \(x) max(as.numeric(x)))) * 
    (sapply(blues, \(x) max(as.numeric(x))))
  sum(power)
}
solve2("inputs/input2.txt") 

# first challenge here is parsing the input file
# I overthought this for a while and tried to parse into a nice data.frame, 
# but that is totally unnecessary and the crux here is just to use regex to get
# the numbers.

# use a lookahead and lookbehind regex to do this.