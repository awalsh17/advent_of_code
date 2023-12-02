# day1

input <- readLines("~/Downloads/input1.txt")
test <- readLines("~/Downloads/input1test.txt")

solve1 <- function(input) {
  # regex for the first digit in each line
  first <- stringr::str_extract(input, "[0-9]")
  last <- stringr::str_extract_all(input, "[0-9]")
  last <- unlist(lapply(last, \(x) x[length(x)]))
  sum(as.numeric(paste0(first, last)))
}
solve1(input)


solve2 <- function(input) {
  # regex for the first digit in each line
  first <- stringr::str_extract(input, "[0-9]|one|two|three|four|five|six|seven|eight|nine")
  last <- stringr::str_extract_all(input, ".*([0-9]|one|two|three|four|five|six|seven|eight|nine)")
  last <- stringr::str_extract_all(last, "([0-9]|one|two|three|four|five|six|seven|eight|nine)$")
  # convert text to digits
  first <- stringr::str_replace_all(
    first, 
    c("one" = "1", "two" = "2", "three" = "3", "four" = "4", "five" = "5", "six" = "6", "seven" = "7", "eight" = "8", "nine" = "9"))
  last <- stringr::str_replace_all(
    unlist(last), 
    c("one" = "1", "two" = "2", "three" = "3", "four" = "4", "five" = "5", "six" = "6", "seven" = "7", "eight" = "8", "nine" = "9"))
  # return(paste0(first, last))
  sum(as.numeric(paste0(first, last)))
}
solve2(input) # 54506 is too high

# The tricky part here is that str_extract_all doesnt deal with overlaps
# So the example was missing something like "oneight" 
# I dealt with this by separating the extraction of the last digit in two steps
# first extract anything up to the digits (.*), then extract the last digit from that