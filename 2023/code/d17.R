# day 17
library(igraph)

# get the data -----
source("R/get_input.R")
input <- get_input("17")
test <- c(
 "2413432311323",
 "3215453535623",
 "3255245654254",
 "3446585845452",
 "4546657867536",
 "1438598798454",
 "4457876987766",
 "3637877979653",
 "4654967986887",
 "4564679986453",
 "1224686865563",
 "2546548887735",
 "4322674655533"
)
# make a graph with edge weights
# this is super inefficient - so I should redo this. All I need is adj matrix!
parse_input <- function(input) {
 width <- nchar(test[1])
 gridi <- test |> strsplit(split = "") |> unlist() |> matrix(ncol = width, byrow = TRUE)
 all_edges <- data.frame()
 for (i in 1:ncol(gridi)) {
  for (j in 1:nrow(gridi)) {
   if (i < ncol(gridi)) all_edges <- rbind(all_edges, c(paste0(i,".",j), paste0(i+1,".",j), gridi[i+1, j], "R"))
   if (j < nrow(gridi)) all_edges <- rbind(all_edges, c(paste0(i,".",j), paste0(i,".",j+1), gridi[i, j+1], "D"))
   if (i > 1) all_edges <- rbind(all_edges, c(paste0(i,".",j), paste0(i-1,".",j), gridi[i-1, j], "L"))
   if (j > 1) all_edges <- rbind(all_edges, c(paste0(i,".",j), paste0(i,".",j-1), gridi[i, j-1], "R"))
  }
 }
 names(all_edges) <- c("from", "to", "weight", "dir")
 all_edges$weight <- as.numeric(all_edges$weight)
 g <- graph_from_data_frame(all_edges, directed = TRUE)
 adj_graph <- igraph::as_adj(g, attr = "weight", sparse = F)
 return(adj_graph)
}

test_adj <- parse_input(test)
adj_graph <- parse_input(input)

# part 1 ------
# hm. now to get the paths from 1.1 to width.width
# that wont be correct because you cannot go more than three in one direction
shortest.paths(g, "1.1", "141.141", algorithm = "dijkstra")

# use new function
source("2023/code/dijkstra_steps.R")
dijkstra_steps(test_adj, start = 1, stop = 5, max_steps = 3)
dijkstra_steps(adj_graph, start = 1, stop = 19881, max_steps = 3)
# example answer is 102
# get 858 on the real input. added 3 to get 861 (too low) 907 is too high
# answer is 866

# part 2 ------

# deep thoughts ------
# shortest path with edge weights seems to be the way. Adding in the complexity
# that you cannot go more than three steps in one direction
# I have a coded dijkstras in R. I tried to modify to track the past directions
# but then decided to just take more than one step at a time