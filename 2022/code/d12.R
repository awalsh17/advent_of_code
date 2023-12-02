# --- Day 12 ---
library(igraph)
real <- read.table("input/input12.txt", sep = "") |>
  tidyr::separate(V1, into = paste0("X", 1:174), sep = "")
real <- real[, -1]
dummy <- read.table("input/dummy12.txt", sep = "") |>
  tidyr::separate(V1, into = paste0("X", 1:9), sep = "")
dummy <- dummy[, -1]

# fewest steps... feels like the first dijkstra's algo puzzle!
# be smarter and use igraph package

# pseudo code:
# turn input into graph - the edge weights are the letter differences
# only keep edges with edge weight <= 1
# find the shortest path from S to E 

# notes:
# parsing the input is very messy here. I am sure there is a much nicer way

make_aoc_graph <- function(input){
  # make graph
  # convert matrix to edge list - anything up/down or left/right
  dim_in <- dim(input)
  # convert matrix to locations
  colnames(input) <- 1:dim_in[2]
  input_long <- stack(input)
  names(input_long) <- c("values", "col")
  input_long$row <- (0:(nrow(input_long) - 1)) %% dim_in[1] + 1
  input_long$col <- as.numeric(input_long$col)
  # all possible moves (this is slow!)
  edges <- tidyr::crossing(r1 = 1:dim_in[1], c1 = 1:dim_in[2],
                           r2 = 1:dim_in[1], c2 = 1:dim_in[2]) |>
    dplyr::mutate(source = paste(r1, c1, sep= "."),
                  sink = paste(r2, c2, sep= ".")) |>
    dplyr::filter(
      source != sink,
      abs(r2 - r1) < 2, 
      abs(c2 - c1) < 2,
      !((abs(r2 - r1) == 1) & (abs(c2 - c1) == 1))) |>
    dplyr::left_join(input_long, by = c("r1" = "row", "c1" = "col")) |>
    dplyr::left_join(input_long, by = c("r2" = "row", "c2" = "col")) |>
    dplyr::mutate(source = ifelse(values.x %in% c("S", "E"), values.x, source),
                  sink = ifelse(values.y %in% c("S", "E"), values.y, sink)) |>
    dplyr::mutate(height.x = ifelse(values.x == "S", 1, as.numeric(factor(values.x, levels = letters))),
                  height.y = ifelse(values.y == "S", 1, as.numeric(factor(values.y, levels = letters))),
                  height.x = ifelse(values.x == "E", 26, height.x),
                  height.y = ifelse(values.y == "E", 26, height.y),
                  weight = height.y - height.x) |>
    dplyr::filter(weight <= 1)
    
  # add values from matrix, calc edges
  input_graph <- graph_from_edgelist(
    as.matrix(edges[, c("source", "sink")]))
  # add edge and node annotation
  # E(input_graph)$weight <- edges$weight
  return(list(igraph = input_graph, input_long = input_long))
}
input_graph <- make_aoc_graph(real) # slower than I expected 

# part 1 - from S to E
length(shortest_paths(input_graph$igraph, from = "S", to = "E", weights = NA)$vpath[[1]]) - 1 

# part 2, fewest from any a to E
# get all the "a" - it is a LOT
a_nodes <- dplyr::filter(input_graph$input_long, values == "a") |>
  dplyr::mutate(node = paste(row, col, sep = ".")) |> dplyr::pull(node)

all_short <- purrr::map(a_nodes, ~length(shortest_paths(input_graph$igraph, from = .x, to = "E", weights = NA)$vpath[[1]]) )
# was 0 when path did not exist
min(unlist(all_short[all_short > 0]) - 1)
