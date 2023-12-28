# code up a search that is modified dijkstras
# https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
#' graph is adjacency matrix
#' start is index of start in that matrix
dijkstra_steps <- function(graph, start, stop, max_steps = 3) {
 # normally, you just track visited and distance
 # here we need visited, distance, direction, steps taken in that direction
 graph_width <- sqrt(nrow(graph))
 node_names <- rownames(graph)
 distances <- rep(Inf, nrow(graph))
 visited <- rep(FALSE, nrow(graph))
 # the previous nodes
 prev <- rep(NA, nrow(graph))
 prev_index <- 1
 # need to also track direction into node (because you need to be diff)
 dir_visited <- rep(0, nrow(graph)) # N 1, S 2, E 3, W 4
 dir_steps <- rep(1, nrow(graph))
 # start with 0
 distances[start] <- 0
 # map the directions
 map_dir <- data.frame(x = c(1,-1,0,0), y = c(0,0,1,-1))
 
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
   # return(list("dist" = distances, "prev" = prev)) # debug
   return(distances[stop])
  }
  # Get the *valid* neighbors
  shortest_neighbors <- which(graph[shortest_index, ] > 0)
 
  for (i in shortest_neighbors) {
   # what direction would these be?
   dir_proposed <- as.numeric(strsplit(node_names[shortest_index], "\\.")[[1]]) - 
    as.numeric(strsplit(node_names[i], "\\.")[[1]])
   new_dir <- which(colSums(t(map_dir) == dir_proposed) == ncol(map_dir))
   if ((new_dir != dir_visited[shortest_index]) | 
       (new_dir == dir_visited[shortest_index] & dir_steps[shortest_index] < max_steps)) {
    # ...if the path over this edge is shorter...
    if (distances[i] > (distances[shortest_index] + graph[shortest_index, i])) {
     # ...save this path as new shortest path.
     distances[i] <- distances[shortest_index] + graph[shortest_index, i]
     prev[i] <- rownames(graph)[shortest_index]
     # ...update the direction and steps in that direction
     if (new_dir == dir_visited[shortest_index]) {
      dir_steps[i] <- dir_steps[shortest_index] + 1
      dir_visited[i] <- new_dir
     } else {
      dir_steps[i] <- 1
      dir_visited[i] <- new_dir
     }
     
     # debug
     # print(paste("current node:", node_names[shortest_index]))
     # print(paste("node dir:", dir_visited[shortest_index]))
     # print(paste("node dir steps:", dir_steps[shortest_index]))
     
    }
   }
   
  }
  
  # we are finished with this node.
  visited[shortest_index] <- TRUE
 }
}

