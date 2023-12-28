# day 25
library(igraph)
# get the data -----
source("R/get_input.R")
input <- get_input("25")

# part 1 ------
# make the network - remove 3 edges to get two components
parse_input <- function(lines) {
 edges <- data.frame("from" = character(), "to" = character())
 for (x in lines) {
  all_nodes <- strsplit(x, ": | ")[[1]]
  from_node <- all_nodes[1]
  to_nodes <- all_nodes[-1]
  new_edges <- data.frame("from" = from_node, "to"= to_nodes)
  edges <- rbind(edges, new_edges)
 }
 igraph::graph_from_edgelist(as.matrix(edges), directed = FALSE)
 # edges
}
g <- parse_input(input)
plot(g) # can see that there are only 3 edges connecting them
bt <- edge_betweenness(g)
bt <- data.frame("edge_idx" = 1:length(bt), between = bt)
edges_remove <- bt[order(bt$between, decreasing = T),"edge_idx"][1:3]

new_g <- delete_edges(g, edges_remove)
plot(new_g)
answer <- prod(clusters(new_g)$csize)

# part 2 ------

# just need to get all the stars

# deep thoughts ------
# This would be harder without igraph