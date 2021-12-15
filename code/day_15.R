# Day 15 in R
library(dplyr)
# Input ----
parse_input <- function(input_path, width = 10) {
  test <- read.fwf(here::here(input_path), widths = rep(1, width)) %>%
    mutate(y = 1:n()) %>%
    reshape2::melt(id.vars = "y", variable.name = "x") %>%
    mutate(
      x = as.numeric(sub("V", "", x)),
      dist = ifelse(x == 1 & y == 1, 0, Inf),
      visited = FALSE,
      current_node = FALSE
    )
  test$current_node[1] <- TRUE
  test
}

test <- parse_input("input/day15/test_input.txt", 10)
input <- parse_input("input/day15/test_input.txt", 100)

# Part 1 instructions ----
# score of min path - I know I have to be smarter here to avoid memory issue
# There are too many paths
# https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm

# Solve ----
# I wrote a version myself that worked (slow).
# But then I decided to just igraph (faster)

solve1 <- function(input, startv, stopv) {
  graph_df <- rbind(
    input %>%
      mutate(
        x0 = x - 1,
        y0 = y
      ),
    input %>%
      mutate(
        x0 = x,
        y0 = y - 1
      )
  ) %>%
    mutate(start = paste0(x0, ".", y0), end = paste0(x, ".", y)) %>%
    select(start, end, weight = value)
  thegraph <- igraph::graph_from_data_frame(graph_df)

  shortest <- igraph::distances(thegraph,
    v = startv, to = stopv,
    weights = E(thegraph)$weight,
    algorithm = "dijkstra",
    mode = "out"
  )
  shortest
}

solve1(test, "1.1", "10.10")
solve1(input, "1.1", "100.100")

# Part 2 ----
# add 4 more to right and bottom
# also need to allow "backwards" moves
# Solve ----

make_big_grid <- function(input) {
  new_list <- list()
  rows <- sqrt(nrow(input))
  count <- 1
  for (i in 0:4) {
    for (j in 0:4) {
      new_in <- input %>%
        mutate(
          value = value + i + j,
          value = ifelse(value > 9, value - 9, value), # could use %% 9
          x = x + rows * i,
          y = y + rows * j
        )
      new_list[[count]] <- new_in
      count <- count + 1
    }
  }
  big <- bind_rows(new_list) %>%
    mutate(dist = ifelse(x == 1 & y == 1, 0, Inf))
  unique(big)
}


solve2 <- function(input, startv, stopv) {
  big_input <- make_big_grid(input)
  graph_df <- rbind(
    big_input %>%
      mutate(
        x0 = x - 1,
        y0 = y
      ),
    big_input %>%
      mutate(
        x0 = x + 1,
        y0 = y
      ),
    big_input %>%
      mutate(
        x0 = x,
        y0 = y + 1
      ),
    big_input %>%
      mutate(
        x0 = x,
        y0 = y - 1
      )
  ) %>%
    mutate(start = paste0(x0, ".", y0), end = paste0(x, ".", y)) %>%
    select(start, end, weight = value)
  thegraph <- igraph::graph_from_data_frame(graph_df)
  shortest <- igraph::distances(thegraph,
    v = startv, to = stopv,
    weights = E(thegraph)$weight,
    algorithm = "dijkstra",
    mode = "out"
  )
  shortest
}

solve2(test, "1.1", "50.50")
solve2(input, "1.1", "500.500")

# messy homemade version =====

do_step <- function(input) {
  final <- input %>%
    mutate(
      current_node = (dist == min(dist[!visited])),
      visited = visited | current_node
    )

  # make some data.frames
  moves <- rbind(
    final %>%
      mutate(
        x0 = x - 1,
        y0 = y
      ),
    final %>%
      mutate(
        x0 = x + 1,
        y0 = y
      ),
    final %>%
      mutate(
        x0 = x,
        y0 = y + 1
      ),
    final %>%
      mutate(
        x0 = x,
        y0 = y - 1
      )
  )

  next_moves <- final %>%
    filter(current_node) %>%
    distinct(x0 = x, y0 = y, dist0 = dist, value0 = value) %>%
    left_join(moves, by = c("x0", "y0")) %>%
    mutate(
      alt = dist0 + value,
      condition = alt < dist,
      dist = ifelse(condition, alt, dist)
    )
  # print(next_moves)
  next_moves <- next_moves %>%
    select(x, y, value, dist, visited, current_node)

  final <- rbind(next_moves, final) %>%
    distinct(x, y, .keep_all = TRUE)
  final
}


df <- test
while (!df[df$x == 10 & df$y == 10, "visited"]) {
  df <- do_step(df)
}
df[df$x == 10 & df$y == 10, "dist"]


df <- input
while (!df[df$x == 100 & df$y == 100, "visited"]) {
  df <- do_step(df)
}
# answer
df[df$x == 100 & df$y == 100, "dist"]

# Part 2

df <- make_big_grid(input)
while (!df[df$x == 500 & df$y == 500, "visited"]) {
  df <- do_step(df)
}
# answer
df[df$x == 500 & df$y == 500, "dist"]
