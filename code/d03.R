# --- Day 3: Rucksack Reorganization ---
library(dplyr)

# lol - R is bad at strings
# but I LOVED THIS ONE

real <- readLines("input/input03.txt")
dummy <- readLines("input/dummy03.txt") 

solve1 <- function(input) {
  # split into compartments
  data.frame(input) %>% 
    mutate(len = stringr::str_length(input)/2,
           part1 = stringr::str_sub(input, start = 1L, end = len),
           part2 = stringr::str_sub(input, start = len + 1),
           common = stringr::str_extract(part1, paste0("[", part2, "]")),
           score = as.numeric(factor(common, levels = c(letters, LETTERS)))
    ) %>% 
    summarise(answer = sum(score))
}
solve1(real)

solve2 <- function(input) {
  # split into compartments
  n_groups <- length(input) / 3
  data.frame(input) %>% 
    # get the groups of elves
    mutate(groupid = sort(rep(1:n_groups, 3)),
           elfid = paste0("part", rep(1:3, n_groups))) %>%
    tidyr::separate_rows(input, sep = "") %>% 
    distinct(groupid, elfid, input) %>%
    count(groupid, input) %>% 
    filter(input != "", n == 3) %>%
    mutate(score = as.numeric(factor(input, levels = c(letters, LETTERS)))
             ) %>% 
    summarise(answer = sum(score))
}
solve2(real)
# ugly - first answer too low: 1503