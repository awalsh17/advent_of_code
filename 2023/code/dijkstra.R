# code up a search that is modified dijkstras
# https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
#' graph is adjacency matrix
#' start is index of start in that matrix
dijkstra <- function(graph, start, stop) {
 distances <- rep(Inf, nrow(graph))
 visited <- rep(FALSE, nrow(graph))
 # the previous nodes
 prev <- rep(NA, nrow(graph))
 prev_index <- 1
 # start with 0
 distances[start] <- 0
 
 repeat{
  # Stop
  if (all(visited)) {
   # return(list("dist" = distances, "prev" = prev))
   return(distances[stop])
  }
  
  # get the next node to visit
  unvis_distances <- distances
  unvis_distances[visited] <- Inf
  shortest_index <- which.min(unvis_distances)
  
  if (shortest_index == stop) {
   # return(list("dist" = distances, "prev" = prev))
   return(distances[stop])
  }
  
  # get the neighbors
  shortest_neighbors <- which(graph[shortest_index, ] > 0)
  
  for (i in shortest_neighbors) {
   # ...if the path over this edge is shorter...
   if (distances[i] > (distances[shortest_index] + graph[shortest_index, i])) {
    # ...save this path as new shortest path.
    distances[i] <- distances[shortest_index] + graph[shortest_index, i]
    prev[i] <- rownames(graph)[shortest_index]
   }
  }
  
  # we are finished with this node.
  visited[shortest_index] <- TRUE
 }
}

dijkstra(adj_graph, start = 1, stop = 169)
