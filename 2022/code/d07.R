# --- Day 7 ---
library(dplyr)
real <- readLines("input/input07.txt")
dummy <- readLines("input/dummy07.txt")

# Find all of the directories with a total size of at most 100000.
# What is the sum of the total sizes of those directories?

# prob need recursion?
# this one was a bit evil because the dir names are not unique!
# so a dir "abcd" is not unique and you need to track the paths
# first work out the paths - then add up things

# a dir is a unique path
get_paths <- function(input) {
  all_paths <- list()
  dir_sizes <- list()
  current_path <- c()
  counter <- 1
  for (line in input) {
    if (stringr::str_detect(line, "\\$ cd \\.\\.")) {
      all_paths[[counter]] <- current_path
      counter <- counter + 1
      current_path <- current_path[-length(current_path)]
    } else if (stringr::str_detect(line, "\\$ cd")) {
      current_dir <- stringr::str_split(line, " ")[[1]][3]
      current_path <- c(current_path, current_dir)
      current_path_name <- paste(current_path, collapse = ".")
      # initialize the size!
      if (!current_path_name %in% names(dir_sizes)) {
        dir_sizes[[current_path_name]] <- 0
      }
    }
    if (stringr::str_starts(line, "[0-9]")) {
      current_path_name <- paste(current_path, collapse = ".")
      new_value <- as.numeric(stringr::str_split(line, " ", simplify = TRUE)[, 1])
      # add to the current dir
      dir_sizes[[current_path_name]] <- dir_sizes[[current_path_name]] +
        new_value

      # add to all the dirs above it
      if (length(current_path) > 1) {
        all_other_path_names <- sapply(
          c(1:(length(current_path) - 1)),
          function(x) paste(current_path[seq_len(x)], collapse = ".")
        )
        for (path in all_other_path_names) {
          dir_sizes[[path]] <- dir_sizes[[path]] +
            new_value
        }
      }
    }
  }
  all_paths[[counter]] <- current_path
  counter <- counter + 1
  return(list(all_paths, dir_sizes))
}
test <- get_paths(real)

solve1 <- function(input) {
  sizes <- get_paths(input)[[2]]
  sum(unlist(sizes[sizes < 100000]))
}
solve1(real) # 1781652 is too low

# Find the smallest directory that, if deleted,
# would free up enough space on the filesystem to run the update.
# What is the total size of that directory?
solve2 <- function(input) {
  sizes <- get_paths(input)[[2]]
  unused <- 70000000 - sizes[[1]]
  min_needed <- 30000000 - unused
  all_options <- sizes[sizes >= min_needed]
  min(unlist(all_options))
}
solve2(real)
