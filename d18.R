# day 18

# get the data -----
source("R/get_input.R")
input <- get_input("18")
test <- c(
"R 6 (#70c710)",
"D 5 (#0dc571)",
"L 2 (#5713f0)",
"D 2 (#d2c081)",
"R 2 (#59c680)",
"D 2 (#411b91)",
"L 5 (#8ceee2)",
"U 2 (#caa173)",
"L 1 (#1b58a2)",
"U 2 (#caa171)",
"R 2 (#7807d2)",
"U 3 (#a77fa3)",
"L 2 (#015232)",
"U 2 (#7a21e3)"
)

# part 1 ------

# first get the edge coordinates 
# second get the area
directions <- input |> 
 strsplit(split = " ")
visited_coord <- c(0, 0) # x, y
current_coord <- c(0, 0)
for (d in directions) {
 if (d[1] == "R") current_coord <- current_coord + c(as.numeric(d[2]), 0)
 if (d[1] == "L") current_coord <- current_coord + c(-as.numeric(d[2]), 0)
 if (d[1] == "U") current_coord <- current_coord + c(0, as.numeric(d[2]))
 if (d[1] == "D") current_coord <- current_coord + c(0, -as.numeric(d[2]))
 visited_coord <- c(visited_coord, current_coord)
}
vertices <- visited_coord |> matrix(ncol = 2, byrow = TRUE)
count_edges <- sum(sapply(directions, \(x) as.numeric(x[2])))

#' Use shoelace to get area
#'
#' @param data matrix or data.frame with x, y
#'
#' @return area 
#'
#' @examples
#' polygon <- data.frame(x = c(1, 2, 1), y = c(1, 3, 4))
#' shoelace_area(polygon)
shoelace_area <- function(data) {
 # sum the determinants
 size <- nrow(data) - 1
 indices <- split(rep((1:2), size) + rep(0:(size-1), each = 2),
                  rep(1:(size), each = 2))
 abs(sum(sapply(indices, \(x) det(data[x, ]))) / 2)
}

answer <- count_edges/2 + shoelace_area(vertices) + 1
# 47045

# part 2 ------
directions <- input |> 
 stringr::str_extract("(?>#)[a-z0-9]+") |> 
 data.frame(all = _) |> 
 dplyr::mutate(dir = substr(all, 7, 7),
               dist = strtoi(substr(all, 2, 6), base = 16L))
visited_coord <- c(0, 0) # x, y
current_coord <- c(0, 0)
for (d in 1:nrow(directions)) {
 if (directions$dir[d] == "0") current_coord <- current_coord + 
   c(as.numeric(directions$dist[d]), 0)
 if (directions$dir[d] == "2") current_coord <- current_coord + 
   c(-as.numeric(directions$dist[d]), 0)
 if (directions$dir[d] == "3") current_coord <- current_coord + 
   c(0, as.numeric(directions$dist[d]))
 if (directions$dir[d] == "1") current_coord <- current_coord + 
   c(0, -as.numeric(directions$dist[d]))
 visited_coord <- c(visited_coord, current_coord)
}
vertices <- visited_coord |> matrix(ncol = 2, byrow = TRUE)
count_edges <- sum(directions$dist)
answer <- count_edges/2 + shoelace_area(vertices) + 1
options(scipen = 999)
# 147839570293376

# deep thoughts ------
# I only knew what to do based on reading solutions to an earlier days puzzle
