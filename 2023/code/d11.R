# day 11
library(dplyr)

# get data and parse it
source("R/get_input.R")
input <- get_input("11")

# hm - another grid/graph one? 

grid_tidy <- data.frame(
  row = rep(1:length(input), each = length(input)),
  col = rep(1:length(input), length(input)),
  value = strsplit(paste(input, collapse = ""), split = "")[[1]]
) 
test <- readLines("2023/inputs/input11test")
test <- data.frame(
  row = rep(1:length(test), each = length(test)),
  col = rep(1:length(test), length(test)),
  value = strsplit(paste(test, collapse = ""), split = "")[[1]]
) 

# part 1 ------
solve1 <- function(input) {
  # expand the universe 
  locations <- input |> 
    group_by(row) |> 
    mutate(expand_row = all(value == ".")) |> 
    group_by(col) |> 
    mutate(expand_col = all(value == ".")) |> 
    mutate(add_row = cumsum(expand_row)) |> 
    group_by(row) |> 
    mutate(add_col = cumsum(expand_col)) |>
    ungroup() |> 
    mutate(row_new = row + add_row, col_new = col + add_col) |>
    filter(value == "#") |> 
    select(value, row_new, col_new) 
  
  # do some dist (manhattan) here
  dist(locations[, c(2:3)], method = "manhattan") |> sum()
}

solve1(test) # correct
solve1(grid_tidy)

# part 2 -----

solve2 <- function(input, expand = 1000000) {
  # expand the universe 
  locations <- input |> 
    group_by(row) |> 
    mutate(expand_row = all(value == ".")) |> 
    group_by(col) |> 
    mutate(expand_col = all(value == ".")) |> 
    mutate(add_row = cumsum(expand_row)) |> 
    group_by(row) |> 
    mutate(add_col = cumsum(expand_col)) |>
    ungroup() |> 
    mutate(row_new = row + add_row*(expand-1), col_new = col + add_col*(expand-1)) |>
    filter(value == "#") |> 
    select(value, row_new, col_new) 
  
  # do some dist (manhattan) here
  dist(locations[, c(2:3)], method = "manhattan") |> sum()
}
solve2(test, expand = 100) 
solve2(grid_tidy, expand = 1000000) 


# thoughts: was easier for me to do this tidyverse-y than with matrices
# part 2 was tricky to figure out the formula to add the rows and columns
