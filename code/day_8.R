# Day 8 in R
library(dplyr)

# Input ----
input <- data.frame(raw = readLines(here::here('input/day8/input.txt')))
test_input <- data.frame(raw = readLines(here::here('input/day8/test_input.txt')))

# Part 1 instructions ----
# It's a decoder - I like it!
# Solve ----
#parse input

solve1 <- function(input){
  parsed <- input %>%
    tidyr::separate(raw,
                    into=c(paste0("r",1:10),paste0("o",1:4)),
                    sep=" \\| |\\s")
  parsed <- as.data.frame(apply(parsed, 1:2, nchar))
  parsed <- apply(parsed, 1:2, as.numeric)
  sum(parsed[,paste0("o",1:4)]%in% c(2,3,4,7))
}
solve1(input) #383

# Part 2 ----
# What do you get if you add up all of the output values?
# Solve ----

# I truly became unhinged with this solution - there is probably a matrix solve or inverse or something

# convert a string of letters to a vector of 0/1
map_letters_to_vector <- function(entry){
  res = letters[1:7] %in% sort(strsplit(entry,"")[[1]])
  sapply(res, function(x) ifelse(x, 1, 0))
}
# get the digit with a vector of 1/0 and the matrix of 1/0
get_digit <- function(input_mat, new_digit){
  # matrix of letters for each digit
  match(new_digit,
        do.call(paste, as.data.frame(input_mat)))-1
}
# for a line, return the correct 4 digit number
get_number_per_line <- function(input_line){
  parsed <- strsplit(
    strsplit(input_line, " \\| ")[[1]][1],
    "\\s"
  )[[1]]
  # matrix of left side of input
  # we re-sort this matrix to be 0:9
  left <- lapply(parsed, map_letters_to_vector)
  left <- t(as.matrix(data.frame(left)))

  # I feel like there is a matrix math way, but no idea
  # So I brute for idenfity the correct digits
  n <- list()
  e <- colSums(left)==4
  b <- colSums(left)==6
  f <- colSums(left)==9
  n[[1+1]] <- rowSums(left)==2
  n[[4+1]] <- rowSums(left)==4
  n[[7+1]] <- rowSums(left)==3
  n[[8+1]] <- rowSums(left)==7
  c <- colSums(left)==8 & colSums(left[!n[[1+1]],])==7
  n[[2+1]] <- rowSums(left[,!f])== rowSums(left)
  n[[3+1]] <- rowSums(left)==5 & (rowSums(left[,!b])==rowSums(left)) & !n[[2+1]]
  n[[5+1]] <- rowSums(left)==5 & !n[[2+1]] & !n[[3+1]]
  n[[9+1]] <- rowSums(left)==6 & (rowSums(left[,!e])==rowSums(left))
  n[[6+1]] <- rowSums(left)==6 &!n[[9+1]] &(rowSums(left[,!c])==rowSums(left))
  n[[0+1]] <- rowSums(left)==6 &!n[[9+1]] & !n[[6+1]]

  # re-order (left) with the real digit rownames 0:9
  new_mat <- left[sapply(n, function(x) which(x)),]
  rownames(new_mat) <- 0:9

  # get new digits
  parsed_end <- strsplit(
    strsplit(input_line, " \\| ")[[1]][2],
    "\\s"
  )[[1]]
  new_digits <- lapply(parsed_end, map_letters_to_vector)
  new_digits <- sapply(new_digits, paste, collapse=" ")
  res <- as.numeric(
    paste(
      sapply(new_digits, function(x) get_digit(new_mat, x)),
      collapse="")
  )
  res
}
# sum up all the numbers
solve2 <- function(input){
  sum(
    sapply(1:nrow(input), function(x) get_number_per_line(input[x,]))
  )
}

solve2(test_input)
solve2(input)
