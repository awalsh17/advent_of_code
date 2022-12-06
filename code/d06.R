# --- Day 6: Tuning Trouble ---

real <- readLines("input/input06.txt") |> stringr::str_split("", simplify = T)
dummy <- "bvwbjplbgvbhsrlpgdmjqwftvncz" |> stringr::str_split("",simplify = T)

solve1 <- function(input){
  answer <- list()
  for (i in 1:(length(input) - 3)) {
    answer[[i]] <- purrr::map(setNames(letters, letters), ~stringr::str_count(input[i:(i+3)], .x)) |>
      dplyr::bind_cols() |>
      colSums()
    answer[[i]] <- !any(answer[[i]] > 1)
  }
  which(unlist(answer))[1] + 3
}
solve1(real) # slow, who cares

solve2 <- function(input) {
  answer <- list()
  for (i in 1:(length(input) - 13)) {
    answer[[i]] <- purrr::map(setNames(letters, letters), ~stringr::str_count(input[i:(i+13)], .x)) |>
      dplyr::bind_cols() |>
      colSums()
    answer[[i]] <- !any(answer[[i]] > 1)
  }
  which(unlist(answer)) + 13
}
solve2(real)
