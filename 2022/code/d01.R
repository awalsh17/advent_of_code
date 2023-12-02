# day 1 --- Day 1: Calorie Counting ---

# Find the Elf carrying the most Calories. How many total Calories is that Elf carrying?

input <- readr::read_file("input/input01.txt")
input <- strsplit(input, "\n\n")[[1]]
input <- strsplit(input, "\n")

solve1 <- function(input) {
  # get each elf
  max(sapply(input, function(x) sum(as.numeric(x))))
}
solve1(input)

solve2 <- function(input) {
  # get top 3, sum them
  sum(sort(sapply(input, function(x) sum(as.numeric(x))), decreasing = TRUE)[1:3])
}
solve2(input)