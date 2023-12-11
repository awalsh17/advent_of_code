# day3
library(dplyr)

# read.fwf is great for reading in a grid
test <- read.fwf("inputs/input3test.txt", rep(1, 10), comment.char = "")
input <- read.fwf("inputs/input3.txt", rep(1, 140), comment.char = "")

# what are all the unique characters?
unique(unlist(input))

# What is the sum of all of the part numbers in the engine schematic?

reformat_input <- function(input) {
  # reformat the input
  long <- input |>
    mutate(row = 1:nrow(input)) |>
    tidyr::pivot_longer(cols = -row) |>
    mutate(col = as.numeric(sub("V", "", name)))

  # merge in left, right, up, down, and diagonal
  # left
  new <- long |>
    left_join(long |> mutate(col = col - 1), by = c("row", "col")) |>
    rename(value = value.x)
  # right
  new <- new |>
    left_join(long |> mutate(col = col + 1), by = c("row", "col")) |>
    rename(value = value.x)
  new <- new |>
    left_join(long |> mutate(row = row - 1), by = c("row", "col")) |>
    rename(value = value.x)
  new <- new |>
    left_join(long |> mutate(row = row + 1), by = c("row", "col")) |>
    rename(value = value.x)
  new <- new |>
    left_join(long |> mutate(row = row - 1, col = col + 1), by = c("row", "col")) |>
    rename(value = value.x)
  new <- new |>
    left_join(long |> mutate(row = row + 1, col = col + 1), by = c("row", "col")) |>
    rename(value = value.x)
  new <- new |>
    left_join(long |> mutate(row = row - 1, col = col - 1), by = c("row", "col")) |>
    rename(value = value.x)
  new <- new |>
    left_join(long |> mutate(row = row + 1, col = col - 1), by = c("row", "col")) |>
    rename(value = value.x)
  return(new)
}


solve1 <- function(input) {
  new <- reformat_input(input)

  filtered <- new |>
    rowwise() |>
    mutate(keep = any(across(
      starts_with("value"),
      ~ (.x %in% c("*", "%", "&", "$", "/", "@", "+", "-", "#", "="))
    ))) |>
    select(row, col, value, keep) |>
    ungroup()

  # now make them numbers again
  all_numbers <- stringr::str_extract_all(
    paste(filtered$value, collapse = ""), "[0-9]+")[[1]]

  all_positions <- as.data.frame(stringr::str_locate_all(
    paste(filtered$value, collapse = ""),
    "[0-9]+"
  )[[1]])
  all_positions <- all_positions |>
    rowwise() |>
    mutate(good = any(between(
      x = c(1:(dim(input)[1] * dim(input)[2]))[filtered$keep],
      left = start,
      right = end
    )))

  sum(as.numeric(all_numbers[all_positions$good]))
}
solve1(input)

# What is the sum of all of the gear ratios in your engine schematic?
# this is tricky because now need to check that a gear has two numbers
# adjacent and multiply them

solve2 <- function(input_path) {
  # reformat the input
  new <- reformat_input(input)
  # now get the "gears"
  filtered <- new |>
    mutate(
      keep = value == "*",
    ) |>
    select(row, col, keep)
  # expand out to the neighbors of the kept gears
  size <- dim(input)[1]
  # need to do as a list to keep the pairs together
  idx <- as.list(c(1:(size * size))[filtered$keep])
  idx <- lapply(
    idx,
    \(x) c(
      x + 1, x - 1, # left and right
      x + size, x - size, # up and down
      x + size + 1, x + size - 1, # diagonal
      x - size + 1, x - size - 1
    )
  )
  # remove out of range
  idx <- lapply(idx, \(x) x[x > 0 & x <= (size * size)])

  # now make them numbers again
  all_numbers <- stringr::str_extract_all(
    paste(new$value, collapse = ""), "[0-9]+")[[1]]
  all_numbers <- as.numeric(all_numbers)
  all_positions <- as.data.frame(stringr::str_locate_all(
    paste(new$value, collapse = ""),
    "[0-9]+"
  )[[1]])
  all_values <- lapply(
    idx,
    \(x) all_positions |>
      mutate(row_number = row_number()) |>
      rowwise() |> # very slow and could fix
      mutate(
        good = any(between(
          x = c(1:(size * size))[x],
          left = start,
          right = end
        ))
      ) |>
      filter(good == TRUE) |>
      pull(row_number)
  )
  all_values <- all_values[unlist(lapply(all_values, \(x) length(x) == 2))]
  all_values <- sapply(all_values, \(x) all_numbers[x][1] * all_numbers[x][2])
  sum(all_values)
}
solve2(input)


# my original idea was to get a matrix with all positions next to the symbols
# using a kernel, but I think that is overkill
# this was much harder than I thought it would be - likely over complicated things
# there is probably a much simpler way to do this that is 1000x faster
