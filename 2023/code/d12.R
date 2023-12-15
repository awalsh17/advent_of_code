# day 12

library(dplyr)
library(gtools)
# get data and parse it
source("R/get_input.R")
input <- data.frame(start = get_input(12)) |> 
  tidyr::separate(start, into = c("clue", "vals"), sep = " ")
test <- data.frame(start = readLines("2023/inputs/input12")) |> 
  tidyr::separate(start, into = c("clue", "vals"), sep = " ")

# part 1 ------

# get the number of possible ways for a clue to make a value
#' example: get_possible("?###????????", "3,2,1")
get_possible <- function(clue, value) {
  # brute force every combination of . or # for the ? in the clue
  clue_vector <- strsplit(clue, "")[[1]]
  n_qs <- sum(clue_vector == "?")
  all_options <- permutations(2, n_qs, c(".", "#"), repeats.allowed = TRUE)
  all_new_values <- apply(all_options, 1, function(x) {
    # replace the ? with the new values
    new_clue <- clue_vector
    new_clue[new_clue == "?"] <- x
    paste(
      nchar(stringr::str_extract_all(paste0(new_clue, collapse = ""), "#+")[[1]]),
      collapse = ",")
  })
  # how many of the new values match the value?
  sum(all_new_values == value)
}

solve1 <- function(input) {

  results = input |> 
    # strip the "." from the beginning and end of clue
    mutate(clue = stringr::str_remove(clue, "^\\.+")) |>
    mutate(clue = stringr::str_remove(clue, "\\.+$")) |>
    mutate(clue = stringr::str_replace_all(clue, "\\.+", "\\.")) |> 
    mutate(clue_len = nchar(clue)) |> 
    rowwise() |> 
    mutate(
      val_count = stringr::str_count(vals, ","),
      val_len = eval(parse(text = gsub(",","+", vals)))) |>
    ungroup() |> 
    mutate(solutions = case_when(
      clue_len <= (val_count + val_len) ~ 1,
      TRUE ~ 2
    )) # the 1s are correct, but > 1 are wrong
  solutions_one <- results[results$solutions == 1,]
  solutions <- apply(results[results$solutions > 1,], 1, function(x) {
    get_possible(x["clue"], x["vals"])
  })
  # then test how many of the options meet the vals criteria
  # then return the sum of the vals for the options that meet the criteria
  sum(solutions) + nrow(solutions_one)
}

solve1(test) # 21
solve1(input) # 7490

# part 2 ------
solve2 <- function() {
  
}
solve2()

# deep thoughts =====
# I def need something more clever here 
# I looked at hints to see that I will need recursive function with memoisation
# instead of brute force