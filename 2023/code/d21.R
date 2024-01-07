# day 21

# part 1 ------

# this is getting all unique coords reached in 64 steps?
# create graph - run n steps, get those nodes
# issue that you can also backtrack... but I can see that those
# will just be the even step nodes. So you need anything that was an even
# number of steps from "S" within 64 steps

# We will be lazy and use {igraph} again!!
# bfs(graph, root, dist = TRUE)

library(igraph)
library(dplyr)
source("R/get_input.R")
raw <- get_input(21)

# convert to igraph
width <- nchar(raw[1])
gridi <- raw |>
  strsplit(split = "") |>
  unlist() |>
  matrix(ncol = width, byrow = TRUE)

long <- data.frame(all = raw) |>
  mutate(row = row_number()) |>
  mutate(value = stringr::str_split(all, "")) |>
  select(-all) |>
  tidyr::unnest(value) |>
  group_by(row) |>
  mutate(col = row_number()) |>
  ungroup() |>
  mutate(node_id = 1:n())

moves <- bind_rows(
 long |> mutate(row_new = row + 1, col_new = col),
 long |> mutate(row_new = row - 1, col_new = col),
 long |> mutate(row_new = row, col_new = col + 1),
 long |> mutate(row_new = row, col_new = col - 1)
) |> 
 select(row, col, row_new, col_new)
edges <- long |>
  left_join(moves) |> 
  right_join(long |> rename(value2 = value, node_id2 = node_id, row_new = row, col_new = col)) |>
  filter(value != "#", value2 != "#")

g <- graph_from_data_frame(edges[, c("node_id", "node_id2")], directed = FALSE)
# neighborhood(g, order = 1, nodes = 7529)
# S == 8581 in my input
res <- bfs(g, root = "8581", dist = TRUE, unreachable = FALSE)

evens <- function(x) (x %% 2 == 0)
sum(res$dist <= 64 & evens(res$dist), na.rm = TRUE)
# This took a while to figure out. I did many things wrong in various ways
# 3651

# part 2 -----
# The actual number of steps he needs to get today is exactly 26501365.
# There has to be a trick? I cannot just repeat the actual graph and keep doing this
# looking at solutions, it seems that you need to use complex positions
# and know that the steps is some modulo - I was never going to in a million years figure that out
# see for example: https://github.com/rundel/advent_of_code_2023/blob/main/day21/day21.R
