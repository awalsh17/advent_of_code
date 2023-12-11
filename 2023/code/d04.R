# day4

# Did some parsing that could have been simpler
test <- read.delim("inputs/input4test", sep = "|", header = F)
test$V1 <- stringr::str_remove(test$V1, "Card [0-9]+: ")
input <- read.delim("inputs/input4.txt", sep = "|", header = F)
test$V1 <- stringr::str_remove(test$V1, "Card [0-9]+: ")


# How many points are they worth in total?

solve1 <- function(input) {
  # get the winning numbers
  winning_numbers <- lapply(strsplit(input$V1, " "), \(x) as.numeric(x))
  your_numbers <- lapply(strsplit(input$V2, " "), \(x) na.omit(as.numeric(x)))
  powers <- unlist(purrr::map2(winning_numbers, your_numbers, \(x, y) sum(y %in% x)))
  sum(floor(2^(powers - 1)))
}
solve1(input)

# how many total scratchcards do you end up with?

solve2 <- function(input) {
  # repeat from part 1
  winning_numbers <- lapply(strsplit(input$V1, " "), \(x) as.numeric(x))
  your_numbers <- lapply(strsplit(input$V2, " "), \(x) na.omit(as.numeric(x)))
  # run a loop and update the n of each card as you go
  total_cards <- rep(1, nrow(input))
  for (i in seq_along(winning_numbers)) {
    result_i <- sum(your_numbers[[i]] %in% winning_numbers[[i]])
    # update cards
    if (result_i > 0) {
      lower <- i + 1
      upper <- min(i + result_i, nrow(input))
      total_cards[lower:upper] <- total_cards[lower:upper] + total_cards[i]
    }
  }
  sum(total_cards, na.rm = TRUE)
}
solve2(input)

# This felt like an easier one for me - you get warnings with this code that I ignore!
