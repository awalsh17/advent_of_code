# day 9

# get data and parse it
source("R/get_input.R")
input <- data.frame(start = get_input(12)) |> 
  tidyr::separate(start, into = c("clue", "vals"), sep = " ")
test <- data.frame(start = readLines("2023/inputs/input12")) |> 
  tidyr::separate(start, into = c("clue", "vals"), sep = " ")

# part 1 ------
solve1 <- function(input) {

  results = input |> 
    # strip the "." from the beginning and end of clue
    mutate(clue = stringr::str_remove(clue, "^\\.+")) |>
    mutate(clue = stringr::str_remove(clue, "\\.+$")) |>
    mutate(clue = stringr::str_replace_all(clue, "\\.+", "\\.")) |> 
    mutate(clue_len = nchar(clue)) |> 
    rowwise() |> 
    mutate(
      q_count = stringr::str_count(clue, "\\?"),
      dot_count = stringr::str_count(clue, "\\."),
      hash_count = stringr::str_count(clue, "#"),
      val_count = stringr::str_count(vals, ",") + 1,
      val_len = eval(parse(text = gsub(",","+", vals)))) |>
    ungroup() |> 
    mutate(solutions = case_when(
      clue_len <= (val_count + val_len - 1) ~ 1,
      TRUE ~ clue_len - (val_count + val_len - 1)
    )) |> # the 1s are correct, but > 1 are wrong
    # this is wrong, but just getting a placeholder
    
    mutate(solutions = case_when(
      solutions > 1 & hash_count == 0 ~ dot_count + 1,
      solutions > 1 & dot_count > 0 ~ dot_count * dot_count,
      solutions > 1  ~ q_count + 1,
      TRUE ~ solutions
    )) 
  # then test how many of the options meet the vals criteria
  # then return the sum of the vals for the options that meet the criteria
  sum(results$solutions)
}

# write a function that given a clue (string), return the possible combinations


solve1(test)
solve1(input) # 4056 is low

# part 2 ------
solve2 <- function() {
  
}
solve2()

# deep thoughts =====
# We can reduce the string some and we find the are almost all unique.
# We can resolve a lot of the rows as having only one solution
# That reduces the search space to the other rows

# I def need something more clever here - some trick like recursion or memoisation
# we can then search through a bunch of different options without repeating